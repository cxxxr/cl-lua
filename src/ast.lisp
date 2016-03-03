(in-package :cl-user)
(defpackage :cl-lua.ast
  (:use :cl :alexandria)
  (:export
   :make-ast
   :ast-name
   :ast-args
   :ast-void-p
   :block-stats
   :block-retstat
   :block-stats
   :block-retstat
   :return-explist
   :label-name
   :goto-name
   :while-exp
   :while-body
   :repeat-body
   :repeat-exp
   :if-test
   :if-then
   :if-else
   :for-name
   :for-init
   :for-end
   :for-step
   :for-body
   :generic-for-namelist
   :generic-for-explist
   :generic-for-body
   :local-namelist
   :local-explist
   :assign-varlist
   :assign-explist
   :var-name
   :number-value
   :string-value
   :tableconstructor-field-sequence
   :tableconstructor-field-pairs
   :unary-op-name
   :unary-op-exp
   :binary-op-name
   :binary-op-left
   :binary-op-right
   :function-parameters
   :function-body
   :refer-table-key
   :refer-table-value
   :call-function-fun
   :call-function-args
   :call-method-prefix
   :call-method-name
   :call-method-args))
(in-package :cl-lua.ast)

(defvar *ast-names* nil)

(defun make-ast (name linum &rest args)
  (declare (ignore linum))
  (check-type name keyword)
  (assert (member name *ast-names*))
  (cons name args))

(defun ast-name (ast) (car ast))
(defun ast-args (ast) (cdr ast))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-slot-names (name slots)
    (loop :for slot :in slots
          :collect (symbolicate name "-" slot))))

(defmacro define-ast (name &rest slots)
  (with-gensyms (gast)
    (let ((slot-names (gen-slot-names name slots))
          (keyword (intern (string name) :keyword)))
      `(unless (member ,keyword *ast-names*)
         (push ,keyword *ast-names*)
         ,@(loop :for slot-name :in slot-names
                 :for n :from 1 :by 1
                 :collect `(defun ,slot-name (,gast)
                             (,(intern (format nil "~:@(~:R~)" n))
                              (ast-args ,gast))))))))

(define-ast block stats retstat)
(define-ast return explist)
(define-ast label name)
(define-ast goto name)
(define-ast break)
(define-ast while exp body)
(define-ast repeat body exp)
(define-ast if test then else)
(define-ast for name init end step body)
(define-ast generic-for namelist explist body)
(define-ast local namelist explist)
(define-ast assign varlist explist)
(define-ast var name)
(define-ast nil)
(define-ast false)
(define-ast true)
(define-ast number value)
(define-ast string value)
(define-ast tableconstructor field-sequence field-pairs)
(define-ast rest)
(define-ast unary-op name exp)
(define-ast binary-op name left right)
(define-ast function parameters body)
(define-ast refer-table key value)
(define-ast call-function fun args)
(define-ast call-method prefix name args)
(define-ast void)

(defun ast-void-p (ast)
  (eq :void (ast-name ast)))
