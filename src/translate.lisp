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

(defun string-to-variable (string)
  (check-type string string)
  (make-symbol string))

(defun make-env ()
  nil)

(defun extend-env-vars (env names symbols)
  (assert (= (length names) (length symbols)))
  (labels ((f (names symbols)
             (cond ((or (null names) (null symbols))
                    (assert (and (null names) (null symbols)))
                    env)
                   (t
                    (extend-env-var (f (cdr names) (cdr symbols))
                                    (car names)
                                    (car symbols))))))
    (f names symbols)))

(defun extend-env-var (env name symbol &optional functionp)
  (check-type name string)
  (cons (list name symbol functionp) env))

(defun env-find (env name)
  (let ((elt (assoc name env :test #'equal)))
    (values (cadr elt) (caddr elt))))

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
  (let ((label-names
          (loop :for stat :in stats
                :when (eq :label (ast-name stat))
                  :collect (car (ast-args stat)))))
    (let ((form (if (null label-names)
                    `(progn
                       ,@(translate-stats stats))
                    `(tagbody
                        ,@(let ((*label-env*
                                  (extend-env-vars *label-env*
                                                   label-names
                                                   (mapcar #'string-to-variable
                                                           label-names))))
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
              ,(translate-single (last1 explist)))))))

(define-translate-single (:label name)
  (env-find *label-env* name))

(define-translate-single (:goto name)
  (let ((tag (env-find *label-env* name)))
    (if tag
        `(go ,tag)
        (error 'goto-error
               :name name
               :filepos (ast-filepos $ast)))))

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
  (with-gensyms (gi glimit gstep gstart-tag gend-tag)
    (let ((gvar (string-to-variable name)))
      `(let ((,gi ,(translate-single init))
             (,glimit ,(translate-single end))
             (,gstep ,(translate-single step)))
         (if (or (cl-lua.runtime:lua-false-p ,gi)
                 (cl-lua.runtime:lua-false-p ,glimit)
                 (cl-lua.runtime:lua-false-p ,gstep))
             (cl-lua.runtime:runtime-error-form
              ,(ast-filepos $ast)
              "'for' limit must be a number")
             (let ((,gi (- ,gi ,gstep)))
               (tagbody
                  ,gstart-tag
                  (incf ,gi ,gstep)
                  (when (or (and (<= 0 ,gstep) (< ,glimit ,gi))
                            (and (< ,gstep 0) (< ,gi ,glimit)))
                    (go ,gend-tag))
                  ,(let ((*loop-end-tag* gend-tag)
                         (*env* (extend-env-var *env* name gvar)))
                     `(let ((,gvar ,gi))
                        ,(translate-single body)))
                  (go ,gstart-tag)
                  ,gend-tag)))))))

(define-translate-single (:generic-for namelist explist body)
  (with-gensyms (gf gs gvar gstart-tag gend-tag)
    (let ((varlist (mapcar #'string-to-variable namelist)))
      `(multiple-value-bind (,gf ,gs ,gvar)
           ,@(mapcar #'translate-single explist)
         (tagbody
            ,gstart-tag
            (multiple-value-bind ,varlist
                (cl-lua.runtime:lua-call ,(ast-filepos $ast)
                                         ,gf
                                         ,gs
                                         ,gvar)
              (declare (ignorable ,@(cdr varlist)))
              (when (cl-lua.runtime:lua-nil-p ,(car varlist))
                (go ,gend-tag))
              (setf ,gvar ,(car varlist))
              ,(let ((*env* (extend-env-vars *env* namelist varlist)))
                 (translate-single body)))
            (go ,gstart-tag)
            ,gend-tag)))))

(define-translate (:local namelist explist) (rest-stats)
  (let ((symbols (mapcar #'string-to-variable namelist)))
    `((multiple-value-bind ,symbols
          (values ,@(mapcar #'translate-single explist))
        ,@(let ((*env* (extend-env-vars *env*
                                        namelist
                                        symbols)))
            (translate-stats rest-stats))))))

(define-translate-single (:assign varlist explist)
  (let ((tmp-vars (loop :for v :in varlist :collect (gensym))))
    `(multiple-value-bind ,tmp-vars
         (values ,@(mapcar #'translate-single explist))
       ,@(loop :for var1 :in varlist
               :for var2 :in tmp-vars
               :collect `(setf ,(translate-single var1) ,var2)))))

(define-translate-single (:var name)
  (if (string= name "_ENV")
      cl-lua.runtime:+lua-env-name+
      (or (env-find *env* name)
          `(cl-lua.runtime:lua-index
            ,(ast-filepos $ast)
            ,cl-lua.runtime:+lua-env-name+
            ,(string-to-lua-string name)))))

(define-translate-single (:paren exp)
  `(values ,(translate-single exp)))

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
  (with-gensyms (gtable)
    `(let ((,gtable (make-lua-table)))
       ,@(loop :for (k v) :in field-pairs
               :for kcode := (translate-single k)
               :for vcode := (translate-single v)
               :collect `(lua-table-put ,gtable ,kcode ,vcode))
       ,@(loop :for elt :in field-sequence
               :for n :from 1
               :collect `(lua-table-put ,gtable ,n ,(translate-single elt)))
       ,gtable)))

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

(defun gen-function (block-name parameters body)
  (let ((rest-p)
        (names)
        (symbols))
    (dolist (p parameters)
      (cond ((eq p :rest)
             (setq rest-p t))
            (t
             (push p names)
             (push (string-to-variable p) symbols))))
    (let* ((rest-var (if rest-p
                         cl-lua.runtime:+lua-rest-symbol+
                         (gensym))))
      (when rest-p
        (push "..." names)
        (push rest-var symbols))
      `((&optional ,@(mapcar #'(lambda (sym)
                                 `(,sym cl-lua.runtime:+lua-nil+))
                             (reverse symbols))
                   &rest ,rest-var)
        (declare (ignorable ,@symbols)
                 ,(if rest-p
                      `(ignorable ,rest-var)
                      `(ignore ,rest-var)))
        ,(let ((*label-env* (make-env))
               (*env* (extend-env-vars *env* names symbols))
               (*block-name* block-name))
           (translate-single body))))))

(define-translate (:local-function name parameters body) (rest-stats)
  (let* ((fname (string-to-variable name))
         (*env* (extend-env-var *env* name fname t)))
    `((labels ((,fname ,@(gen-function fname parameters body)))
        ,@(translate-stats rest-stats)))))

(define-translate-single (:function parameters body)
  (let ((block-name (gensym)))
    (destructuring-bind (parameters decls &rest body)
        (gen-function block-name parameters body)
      `(lambda ,parameters
         ,decls
         (block ,block-name
           ,@body)))))

(define-translate-single (:index key value)
  `(cl-lua.runtime:lua-index
    ,(ast-filepos $ast)
    ,(translate-single key)
    ,(translate-single value)))

(defun gen-call-local-function (func-name args)
  (if (or (null args)
          (atom (last1 args))
          (and (eq 'values (car (last1 args)))
               (length=1 (cdr (last1 args)))))
      `(,func-name ,@args)
      `(multiple-value-call #',func-name ,@args)))

(define-translate-single (:call-function fun args)
  (let ((args (when args
                (append (mapcar #'(lambda (arg)
                                    `(values ,(translate-single arg)))
                                (butlast args))
                        (list (translate-single (last1 args)))))))
    (block outer
      (when (eq :var (ast-name fun))
        (multiple-value-bind (func-name functionp)
            (env-find *env* (car (ast-args fun)))
          (when functionp
            (return-from outer (gen-call-local-function func-name args)))))
      (let ((func-form (translate-single fun)))
        `(cl-lua.runtime:lua-call ,(ast-filepos $ast)
                                  ,func-form
                                  ,@args)))))

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
    `(cl-lua.runtime:with-runtime ()
       (block ,*block-name*
         ,(translate-single ast)))))
