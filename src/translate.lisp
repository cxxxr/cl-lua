(in-package :cl-user)
(defpackage :cl-lua.translate
  (:use
   :cl
   :alexandria
   :cl-lua.util
   :cl-lua.ast)
  (:export))
(in-package :cl-lua.translate)

(defvar *loop-end-tag*)
(defvar *block-name*)
(defvar *env*)

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

(defgeneric translate-aux (ast name args rest-stats))

(defun translate-concat (form rest-stats)
  (if (null rest-stats)
      form
      (cons form
            (translate-1 (car rest-stats)
                         (cdr rest-stats)))))

(defmacro define-translate ((name &rest parameters) (rest-stats-var)
                            &body body)
  (with-gensyms (gname gargs)
    `(defmethod translate-aux
         ($ast (,gname (eql ,name)) ,gargs ,rest-stats-var)
       (declare (ignorable $ast))
       (destructuring-bind ,parameters ,gargs
         (declare (ignorable ,@parameters))
         ,@body))))

(defmacro define-translate-simple ((name &rest parameters) &body body)
  (with-gensyms (grest-stats)
    `(define-translate (,name ,@parameters) (,grest-stats)
       (translate-concat (progn ,@body)
                         ,grest-stats))))

(defun translate-stats (stats)
  (if (null stats)
      nil
      (translate-concat (translate-1 (car stats))
                        (cdr stats))))

(define-translate (:block stats retstat) (rest-stats)
  `(tagbody
      ,(translate-concat
        (let ((stats (if (ast-void-p retstat)
                         stats
                         (append stats
                                 (list retstat)))))
          (translate-stats stats))
        rest-stats)))

(define-translate (:return explist) (rest-stats)
  (assert (null rest-stats))
  (if (ast-void-p explist)
      `(return-from ,*block-name* nil)
      `(return-from ,*block-name*
         (values ,@(mapcar #'translate-1 explist)))))

(define-translate-simple (:label name)
  (string-to-runtime-symbol name))

(define-translate-simple (:goto name)
  `(go ,(string-to-runtime-symbol name)))

(define-translate-simple (:break)
  (if (boundp '*loop-end-tag*)
      `(go ,*loop-end-tag*)
      (error "not inside a loop")))

(define-translate-simple (:while exp body)
  (with-gensyms (gstart-tag gend-tag)
    `(tagbody
        ,gstart-tag
        (when (is-false ,(translate-1 exp))
          (go ,gend-tag))
        ,(let ((*loop-end-tag* gend-tag))
           (translate-1 body))
        ,gend-tag)))

(define-translate-simple (:repeat body exp)
  (with-gensyms (gstart-tag gend-tag)
    `(tagbody
        ,gstart-tag
        ,(let ((*loop-end-tag* gend-tag))
           (translate-1 body))
        (when (is-false ,(translate-1 exp))
          (go ,gstart-tag))
        ,gend-tag)))

(define-translate-simple (:if test then else)
  `(if (is-true ,(translate-1 test))
       ,(translate-1 then)
       ,(translate-1 else)))

(define-translate-simple (:for name init end step body)
  (with-gensyms (gi glimit gstart-tag gend-tag)
    `(let ((,gi ,(translate-1 init))
           (,glimit ,(translate-1 end)))
       (tagbody
          ,gstart-tag
          (when (> ,gi ,glimit) (go ,gend-tag))
          ,(let ((*loop-end-tag* gend-tag)
                 (*env* (extend-env (list name) *env*)))
             (translate-1 body))
          (incf ,gi)
          (go ,gstart-tag)
          ,gend-tag))))

(define-translate-simple (:generic-for namelist explist body)
  (let* ((vars (mapcar #'string-to-runtime-symbol namelist))
         (var1 (car vars)))
    (with-gensyms (gf gs gvar gstart-tag gend-tag)
      `(multiple-value-bind (,gf ,gs ,gvar)
           (values ,@(mapcar #'translate-1 explist))
         (tagbody
            ,gstart-tag
            (multiple-value-bind ,vars
                ,(gen-call-function gf (list gs gvar))
              (when (cl-lua.runtime:lua-eq
                     ,var1
                     cl-lua.runtime:+lua-nil+)
                (go ,gend-tag))
              (setf ,gvar ,var1)
              ,(let ((*loop-end-tag* gend-tag)
                     (*env* (extend-env namelist *env*)))
                 (translate-1 body)))
            (go ,gstart-tag)
            ,gend-tag)))))

(define-translate (:local namelist explist) (rest-stats)
  `(multiple-value-bind ,(mapcar #'string-to-runtime-symbol namelist)
       (values ,@(mapcar #'translate-1 explist))
     ,(let ((*env* (extend-env namelist *env*)))
        (translate-stats rest-stats))))

(define-translate-simple (:assign varlist explist)
  (let ((tmp-vars (loop :for v :in varlist :collect (gensym))))
    `(multiple-value-bind ,tmp-vars
         (values ,@(mapcar #'translate-1 explist))
       ,@(loop :for var1 :in varlist
               :for var2 :in tmp-vars
               :collect `(setf ,(translate-1 var1) ,var2)))))

(define-translate-simple (:var name)
  (if (env-find *env* name)
      (string-to-runtime-symbol name)
      `(cl-lua.runtime:lua-get-table
        ,cl-lua.runtime:+lua-env-name+
        ,(string-to-bytes name))))

(define-translate-simple (:nil)
  cl-lua.runtime:+lua-nil+)

(define-translate-simple (:false)
  cl-lua.runtime:+lua-false+)

(define-translate-simple (:true)
  cl-lua.runtime:+lua-true+)

(define-translate-simple (:number value)
  value)

(define-translate-simple (:string value)
  value)

(define-translate-simple (:tableconstructor field-sequence field-pairs)
  `(cl-lua.runtime:make-lua-table
    :pairs (list
            ,@(mapcar #'(lambda (elt)
                          `(cons ,(translate-1 (car elt))
                                 ,(translate-1 (cadr elt))))
                      field-pairs))
    :sequence (vector ,@(mapcar #'translate-1 field-sequence))))

(define-translate-simple (:rest)
  cl-lua.runtime:+lua-rest-symbol+)

(define-translate-simple (:unary-op name exp)
  (eswitch (name :test #'string=)
    ("-"
     `(cl-lua.runtime:lua-minus ,(translate-1 exp)))
    ("not"
     `(cl-lua.runtime:lua-not ,(translate-1 exp)))
    ("#"
     `(cl-lua.runtime:lua-len ,(translate-1 exp)))
    ("~"
     `(cl-lua.runtime:lua-lognot-unary ,(translate-1 exp)))))

(define-translate-simple (:binary-op name left right)
  (let ((left-form (translate-1 left))
        (right-form (translate-1 right)))
    (eswitch (name :test #'string=)
      ("+"
       `(cl-lua.runtime:lua-add ,left-form ,right-form))
      ("-"
       `(cl-lua.runtime:lua-sub ,left-form ,right-form))
      ("*"
       `(cl-lua.runtime:lua-mul ,left-form ,right-form))
      ("/"
       `(cl-lua.runtime:lua-div ,left-form ,right-form))
      ("//"
       `(cl-lua.runtime:lua-ndiv ,left-form ,right-form))
      ("^"
       `(cl-lua.runtime:lua-pow ,left-form ,right-form))
      ("%"
       `(cl-lua.runtime:lua-mod ,left-form ,right-form))
      ("&"
       `(cl-lua.runtime:lua-logand ,left-form ,right-form))
      ("~"
       `(cl-lua.runtime:lua-lognot ,left-form ,right-form))
      ("|"
       `(cl-lua.runtime:lua-logior ,left-form ,right-form))
      (">>"
       `(cl-lua.runtime:lua-rshift,left-form ,right-form))
      ("<<"
       `(cl-lua.runtime:lua-lshift ,left-form ,right-form))
      (".."
       `(cl-lua.runtime:lua-concat ,left-form ,right-form))
      ("<"
       `(cl-lua.runtime:lua-lt ,left-form ,right-form))
      ("<="
       `(cl-lua.runtime:lua-le ,left-form ,right-form))
      (">"
       `(cl-lua.runtime:lua-gt ,left-form ,right-form))
      (">="
       `(cl-lua.runtime:lua-ge ,left-form ,right-form))
      ("=="
       `(cl-lua.runtime:lua-eq ,left-form ,right-form))
      ("~="
       `(cl-lua.runtime:lua-ne ,left-form ,right-form))
      ("and"
       `(cl-lua.runtime:lua-and ,left-form ,right-form))
      ("or"
       `(cl-lua.runtime:lua-or ,left-form ,right-form)))))

(define-translate-simple (:function parameters body)
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
           ,(translate-1 body))))))

(define-translate-simple (:refer-table key value)
  `(cl-lua.runtime:lua-get-table
    ,(translate-1 key)
    ,(translate-1 value)))

(defun gen-call-function (fun args)
  `(multiple-value-call ,fun ,@args))

(define-translate-simple (:call-function fun args)
  (gen-call-function (translate-1 fun)
                     (mapcar #'translate-1 args)))

(define-translate-simple (:call-method prefix name args)
  (with-gensyms (gvalue)
    `(let ((,gvalue ,(translate-1 prefix)))
       (multiple-value-call
           (cl-lua.runtime:lua-get-table ,gvalue
                                         ,(string-to-bytes name))
         ,gvalue
         ,@(mapcar #'translate-1 args)))))

(define-translate-simple (:void))

(defun translate-1 (ast &optional rest-stats)
  (translate-aux ast
                 (ast-name ast)
                 (ast-args ast)
                 rest-stats))

(defun translate (ast)
  (print ast)
  (let ((*block-name* nil)
        (*env* (make-env)))
    `(block ,*block-name*
       ,(translate-1 ast))))
