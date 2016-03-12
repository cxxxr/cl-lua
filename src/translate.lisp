(in-package :cl-user)
(defpackage :cl-lua.translate
  (:use
   :cl
   :cl-lua.util
   :cl-lua.ast
   :cl-lua.error
   :cl-lua.lua-object)
  (:import-from
   :alexandria
   :symbolicate
   :with-gensyms
   :eswitch)
  (:export
   :translate))
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

(defun extend-env-vars (vars env)
  (append vars env))

(defun extend-env-var (var env)
  (cons var env))

(defun env-find (env var)
  (find var env :test #'equal))

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
    (let ((form (if (null label-list)
                    `(progn
                       ,@(translate-stats stats))
                    `(tagbody
                        ,@(let ((*label-env*
                                  (extend-env-vars label-list *label-env*)))
                            (translate-stats stats))))))
      (translate-concat form
                        rest-stats))))

(define-translate-single (:return explist)
  (cond ((null explist)
         `(return-from ,*block-name* cl-lua.runtime:+lua-nil+))
        ((length=1 explist)
         `(return-from ,*block-name*
            ,(translate-single (car explist))))
        (t
         `(return-from ,*block-name*
            (multiple-value-call #'values
              ,@(mapcar #'(lambda (arg)
                            `(values ,(translate-single arg)))
                        (butlast explist))
              ,(translate-single (car (last explist))))))))

(define-translate-single (:label name)
  (string-to-runtime-symbol name))

(define-translate-single (:goto name)
  (if (env-find *label-env* name)
      `(go ,(string-to-runtime-symbol name))
      (error 'goto-error
             :name name
             :filepos (ast-filepos $ast))))

(define-translate-single (:break)
  (if (boundp '*loop-end-tag*)
      `(go ,*loop-end-tag*)
      (error 'break-error
             :filepos (ast-filepos $ast))))

(define-translate-single (:while exp body)
  (with-gensyms (gstart-tag gend-tag)
    `(tagbody
        ,gstart-tag
        (when (cl-lua.runtime:lua-false-p ,(translate-single exp))
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
        (when (cl-lua.runtime:lua-false-p ,(translate-single exp))
          (go ,gstart-tag))
        ,gend-tag)))

(define-translate-single (:if test then else)
  `(if (cl-lua.runtime:lua-false-p ,(translate-single test))
       ,(translate-single else)
       ,(translate-single then)))

(define-translate-single (:for name init end step body)
  (with-gensyms (gi glimit gstart-tag gend-tag)
    `(let ((,gi ,(translate-single init))
           (,glimit ,(translate-single end)))
       (tagbody
          ,gstart-tag
          (when (> ,gi ,glimit) (go ,gend-tag))
          ,(let ((*loop-end-tag* gend-tag)
                 (*env* (extend-env-var name *env*)))
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
                (cl-lua.runtime:lua-call
                 ,(ast-filepos $ast) ,gf ,gs ,gvar)
              (when (cl-lua.runtime:lua-eq
                     ,(ast-filepos $ast)
                     ,var1
                     cl-lua.runtime:+lua-nil+)
                (go ,gend-tag))
              (setf ,gvar ,var1)
              ,(let ((*loop-end-tag* gend-tag)
                     (*env* (extend-env-vars namelist *env*)))
                 (translate-single body)))
            (go ,gstart-tag)
            ,gend-tag)))))

(define-translate (:local namelist explist) (rest-stats)
  `((multiple-value-bind ,(mapcar #'string-to-runtime-symbol namelist)
        (values ,@(mapcar #'translate-single explist))
      ,@(let ((*env* (extend-env-vars namelist *env*)))
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

(define-translate-single (:paren exp)
  `(values ,(translate exp)))

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
  (with-gensyms (gtable-var glength gend-tag)
    (let ((seqlen (length field-sequence)))
      `(let ((,gtable-var (make-hash-table :test #'equalp))
             (,glength ,seqlen))
         ,@(loop :for (k v) :in field-pairs
                 :for kcode := (translate-single k)
                 :for vcode := (translate-single v)
                 :collect `(setf (gethash ,kcode ,gtable-var) ,vcode))
         ,@(loop :for elt :in field-sequence
                 :for n :from 1
                 :collect `(setf (gethash ,n ,gtable-var)
                                 ,(translate-single elt)))
         ,@(unless (null field-pairs)
             `((tagbody
                  ,@(loop :for n
                          :from (1+ seqlen)
                            :to (+ seqlen (length field-pairs))
                          :collect `(if (gethash ,n ,gtable-var)
                                        (incf ,glength)
                                        (go ,gend-tag)))
                  ,gend-tag)))
         (make-lua-table :hash-table ,gtable-var
                         :sequence-length ,glength)))))

(define-translate-single (:rest)
  (if (env-find *env* cl-lua.runtime:+lua-rest-symbol+)
      cl-lua.runtime:+lua-rest-symbol+
      (error 'variadic-error
             :filepos (ast-filepos $ast))))

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

(defun gen-function (parameters body)
  (let ((rest-p))
    (let ((args (loop :for p :in parameters
                      :if (eq p :rest)
                        :do (setq rest-p t)
                      :else
                        :collect (string-to-runtime-symbol p))))
      (let ((rest-var (if rest-p
                          cl-lua.runtime:+lua-rest-symbol+
                          (gensym))))
        `((&optional ,@args &rest ,rest-var)
          (declare (ignorable ,@args)
                   ,@(unless rest-p `((ignore ,rest-var))))
          ,(let ((*label-env* (make-env))
                 (*env* (extend-env-vars (if rest-p
                                             (cons rest-var args)
                                             args)
                                    *env*)))
             (translate-single body)))))))

(define-translate (:local-function name parameters body) (rest-stats)
  (let* ((fname (string-to-runtime-symbol name))
         (*env* (extend-env-var fname *env*)))
    `((labels ((,fname ,@(gen-function parameters body)))
        ,@(translate-stats rest-stats)))))

(define-translate-single (:function parameters body)
  `(lambda ,@(gen-function parameters body)))

(define-translate-single (:index key value)
  `(cl-lua.runtime:lua-index
    ,(ast-filepos $ast)
    ,(translate-single key)
    ,(translate-single value)))

(define-translate-single (:call-function fun args)
  (if (null args)
      `(cl-lua.runtime:lua-call
        ,(ast-filepos $ast)
        ,(translate-single fun))
      `(cl-lua.runtime:lua-call
        ,(ast-filepos $ast)
        ,(translate-single fun)
        ,@(mapcar #'(lambda (arg)
                      `(values ,(translate-single arg)))
                  (butlast args))
        ,(translate-single (car (last args))))))

(define-translate-single (:call-method prefix name args)
  (with-gensyms (gvalue)
    `(let ((,gvalue ,(translate-single prefix)))
       (cl-lua.runtime:lua-call
        ,(ast-filepos $ast)
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
    `(let ((,cl-lua.runtime:+lua-env-name+ (make-lua-table)))
       (declare (ignorable ,cl-lua.runtime:+lua-env-name+))
       (block ,*block-name*
         ,(translate-single ast)))))
