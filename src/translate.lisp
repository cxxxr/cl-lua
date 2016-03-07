(in-package :cl-user)
(defpackage :cl-lua.translate
  (:use
   :cl
   :cl-lua.util
   :cl-lua.ast
   :cl-lua.error)
  (:import-from
   :alexandria
   :symbolicate
   :with-gensyms
   :eswitch)
  (:import-from
   :cl-lua.runtime
   :string-to-lua-string)
  (:export))
(in-package :cl-lua.translate)

(defvar *loop-end-tag*)
(defvar *block-name*)
(defvar *env*)
(defvar *label-env*)

(defvar *translators* (make-hash-table))

(defun string-to-runtime-symbol (string)
  (check-type string string)
  (intern string :cl-lua.runtime))

(defun make-env ()
  nil)

(defun extend-env (vars env)
  (list vars env))

(defun env-find (env var)
  (dolist (vars env)
    (when (find var vars :test #'equal)
      (return t))))

(defun translate-dispatch (ast rest-stats)
  (assert (gethash (ast-name ast) *translators*))
  (funcall (gethash (ast-name ast) *translators*)
           ast
           (ast-args ast)
           rest-stats))

(defun translate-single (ast)
  (car (translate-dispatch ast nil)))

(defun translate-concat (form rest-stats)
  (if (null rest-stats)
      (list form)
      (cons form
            (translate-dispatch (car rest-stats)
                                (cdr rest-stats)))))

(defun translate-stats (stats)
  (if (null stats)
      nil
      (translate-dispatch (car stats)
                          (cdr stats))))

(defmacro define-translate ((name &rest parameters) (rest-stats-var)
                            &body body)
  (with-gensyms (gargs)
    (let ((tr-name (symbolicate "%TRANSLATE-" name)))
      `(progn
         (setf (gethash ,name *translators*) ',tr-name)
         (defun ,tr-name ($ast ,gargs ,rest-stats-var)
           (declare (ignorable $ast))
           (destructuring-bind ,parameters ,gargs
             (declare (ignorable ,@parameters))
             ,@body))))))

(defmacro define-translate-single ((name &rest parameters) &body body)
  (with-gensyms (grest-stats)
    `(define-translate (,name ,@parameters) (,grest-stats)
       (translate-concat (progn ,@body)
                         ,grest-stats))))

(define-translate (:block stats) (rest-stats)
  (let ((label-list
          (loop :for stat :in stats
                :when (eq :label (ast-name stat))
                  :collect (car (ast-args stat)))))
    (let ((form `(tagbody
                    ,@(let ((*label-env* (extend-env label-list *label-env*)))
                        (translate-stats stats)))))
      (translate-concat form
                        rest-stats))))

(define-translate-single (:return explist)
  (if (null explist)
      `(return-from ,*block-name* cl-lua.runtime:+lua-nil+)
      `(return-from ,*block-name*
         (values ,@(mapcar #'translate-single explist)))))

(define-translate-single (:label name)
  (string-to-runtime-symbol name))

(define-translate-single (:goto name)
  (if (env-find *label-env* name)
      `(go ,(string-to-runtime-symbol name))
      (error 'translate-error
             :text (format nil
                           "no visible label ~A for <goto> at line ~D"
                           name
                           (ast-linum $ast)))))

(define-translate-single (:break)
  (if (boundp '*loop-end-tag*)
      `(go ,*loop-end-tag*)
      (error 'translate-error
             :text (format nil
                           "<break> at line ~D not inside a loop"
                           (ast-linum $ast)))))

(define-translate-single (:while exp body)
  (with-gensyms (gstart-tag gend-tag)
    `(tagbody
        ,gstart-tag
        (when (is-false ,(translate-single exp))
          (go ,gend-tag))
        ,(let ((*loop-end-tag* gend-tag))
           (translate-single body))
        ,gend-tag)))

(define-translate-single (:repeat body exp)
  (with-gensyms (gstart-tag gend-tag)
    `(tagbody
        ,gstart-tag
        ,(let ((*loop-end-tag* gend-tag))
           (translate-single body))
        (when (is-false ,(translate-single exp))
          (go ,gstart-tag))
        ,gend-tag)))

(define-translate-single (:if test then else)
  `(if (is-true ,(translate-single test))
       ,(translate-single then)
       ,(translate-single else)))

(define-translate-single (:for name init end step body)
  (with-gensyms (gi glimit gstart-tag gend-tag)
    `(let ((,gi ,(translate-single init))
           (,glimit ,(translate-single end)))
       (tagbody
          ,gstart-tag
          (when (> ,gi ,glimit) (go ,gend-tag))
          ,(let ((*loop-end-tag* gend-tag)
                 (*env* (extend-env (list name) *env*)))
             (translate-single body))
          (incf ,gi)
          (go ,gstart-tag)
          ,gend-tag))))

(define-translate-single (:generic-for namelist explist body)
  (let* ((vars (mapcar #'string-to-runtime-symbol namelist))
         (var1 (car vars)))
    (with-gensyms (gf gs gvar gstart-tag gend-tag)
      `(multiple-value-bind (,gf ,gs ,gvar)
           (values ,@(mapcar #'translate-single explist))
         (tagbody
            ,gstart-tag
            (multiple-value-bind ,vars
                ,(gen-call-function gf (list gs gvar))
              (when (cl-lua.runtime:lua-eq
                     ,(ast-filepos $ast)
                     ,var1
                     cl-lua.runtime:+lua-nil+)
                (go ,gend-tag))
              (setf ,gvar ,var1)
              ,(let ((*loop-end-tag* gend-tag)
                     (*env* (extend-env namelist *env*)))
                 (translate-single body)))
            (go ,gstart-tag)
            ,gend-tag)))))

(define-translate (:local namelist explist) (rest-stats)
  `((multiple-value-bind ,(mapcar #'string-to-runtime-symbol namelist)
        (values ,@(mapcar #'translate-single explist))
      ,(let ((*env* (extend-env namelist *env*)))
         (translate-stats rest-stats)))))

(define-translate-single (:assign varlist explist)
  (let ((tmp-vars (loop :for v :in varlist :collect (gensym))))
    `(multiple-value-bind ,tmp-vars
         (values ,@(mapcar #'translate-single explist))
       ,@(loop :for var1 :in varlist
               :for var2 :in tmp-vars
               :collect `(setf ,(translate-single var1) ,var2)))))

(define-translate-single (:var name)
  (if (env-find *env* name)
      (string-to-runtime-symbol name)
      `(cl-lua.runtime:lua-index
        ,(ast-filepos $ast)
        ,cl-lua.runtime:+lua-env-name+
        ,(string-to-lua-string name))))

(define-translate-single (:nil)
  cl-lua.runtime:+lua-nil+)

(define-translate-single (:false)
  cl-lua.runtime:+lua-false+)

(define-translate-single (:true)
  cl-lua.runtime:+lua-true+)

(define-translate-single (:number value)
  value)

(define-translate-single (:string value)
  value)

(define-translate-single (:tableconstructor field-sequence field-pairs)
  `(cl-lua.runtime:make-lua-table
    ,(ast-filepos $ast)
    :pairs (list
            ,@(mapcar #'(lambda (elt)
                          `(cons ,(translate-single (car elt))
                                 ,(translate-single (cadr elt))))
                      field-pairs))
    :sequence (vector ,@(mapcar #'translate-single field-sequence))))

(define-translate-single (:rest)
  cl-lua.runtime:+lua-rest-symbol+)

(define-translate-single (:unary-op name exp)
  (eswitch (name :test #'string=)
    ("-"
     `(cl-lua.runtime:lua-unm ,(ast-filepos $ast) ,(translate-single exp)))
    ("not"
     `(cl-lua.runtime:lua-not ,(ast-filepos $ast) ,(translate-single exp)))
    ("#"
     `(cl-lua.runtime:lua-len ,(ast-filepos $ast) ,(translate-single exp)))
    ("~"
     `(cl-lua.runtime:lua-bnot ,(ast-filepos $ast) ,(translate-single exp)))))

(define-translate-single (:binary-op name left right)
  (let ((left-form (translate-single left))
        (right-form (translate-single right)))
    (eswitch (name :test #'string=)
      ("+"
       `(cl-lua.runtime:lua-add ,(ast-filepos $ast) ,left-form ,right-form))
      ("-"
       `(cl-lua.runtime:lua-sub ,(ast-filepos $ast) ,left-form ,right-form))
      ("*"
       `(cl-lua.runtime:lua-mul ,(ast-filepos $ast) ,left-form ,right-form))
      ("/"
       `(cl-lua.runtime:lua-div ,(ast-filepos $ast) ,left-form ,right-form))
      ("//"
       `(cl-lua.runtime:lua-idiv ,(ast-filepos $ast) ,left-form ,right-form))
      ("^"
       `(cl-lua.runtime:lua-pow ,(ast-filepos $ast) ,left-form ,right-form))
      ("%"
       `(cl-lua.runtime:lua-mod ,(ast-filepos $ast) ,left-form ,right-form))
      ("&"
       `(cl-lua.runtime:lua-band ,(ast-filepos $ast) ,left-form ,right-form))
      ("~"
       `(cl-lua.runtime:lua-bxor ,(ast-filepos $ast) ,left-form ,right-form))
      ("|"
       `(cl-lua.runtime:lua-bor ,(ast-filepos $ast) ,left-form ,right-form))
      (">>"
       `(cl-lua.runtime:lua-shr,left-form ,(ast-filepos $ast) ,right-form))
      ("<<"
       `(cl-lua.runtime:lua-shl ,(ast-filepos $ast) ,left-form ,right-form))
      (".."
       `(cl-lua.runtime:lua-concat ,(ast-filepos $ast) ,left-form ,right-form))
      ("<"
       `(cl-lua.runtime:lua-lt ,(ast-filepos $ast) ,left-form ,right-form))
      ("<="
       `(cl-lua.runtime:lua-le ,(ast-filepos $ast) ,left-form ,right-form))
      (">"
       `(cl-lua.runtime:lua-gt ,(ast-filepos $ast) ,left-form ,right-form))
      (">="
       `(cl-lua.runtime:lua-ge ,(ast-filepos $ast) ,left-form ,right-form))
      ("=="
       `(cl-lua.runtime:lua-eq ,(ast-filepos $ast) ,left-form ,right-form))
      ("~="
       `(cl-lua.runtime:lua-ne ,(ast-filepos $ast) ,left-form ,right-form))
      ("and"
       `(cl-lua.runtime:lua-and ,(ast-filepos $ast) ,left-form ,right-form))
      ("or"
       `(cl-lua.runtime:lua-or ,(ast-filepos $ast) ,left-form ,right-form)))))

(define-translate-single (:function parameters body)
  (with-gensyms (gargs)
    `(lambda (&rest ,gargs)
       (block nil
         (multiple-value-bind
               ,(mapcan #'(lambda (p)
                            (if (eq p :rest)
                                (list '&rest cl-lua.runtime:+lua-rest-symbol+)
                                (list p)))
                 parameters)
             ,gargs
           ,(let ((*label-env* (make-env)))
              (translate-single body)))))))

(define-translate-single (:index key value)
  `(cl-lua.runtime:lua-index
    ,(ast-filepos $ast)
    ,(translate-single key)
    ,(translate-single value)))

(defun gen-call-function (fun args)
  `(multiple-value-call ,fun ,@args))

(define-translate-single (:call-function fun args)
  (gen-call-function (translate-single fun)
                     (mapcar #'translate-single args)))

(define-translate-single (:call-method prefix name args)
  (with-gensyms (gvalue)
    `(let ((,gvalue ,(translate-single prefix)))
       (multiple-value-call
           (cl-lua.runtime:lua-index ,(ast-filepos $ast)
                                     ,gvalue
                                     ,(string-to-lua-string name))
         ,gvalue
         ,@(mapcar #'translate-single args)))))

(define-translate-single (:void))

(defun translate (ast)
  (let ((*block-name* nil)
        (*env* (make-env))
        (*label-env* (make-env)))
    `(block ,*block-name*
       ,(translate-single ast))))
