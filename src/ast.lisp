(in-package :cl-user)
(defpackage :cl-lua.ast
  (:use :cl)
  (:export :make-ast))
(in-package :cl-lua.ast)

(defstruct (ast (:constructor make-ast-internal))
  op
  linum
  args)

(defmethod print-object ((ast ast) stream)
  (format stream "(~S ~{~S~^ ~})" (ast-op ast) (ast-args ast)))

(defun make-ast (op linum &rest args)
  (check-type op keyword)
  (unless (eq op :void)
    (check-type linum integer))
  (make-ast-internal :op op :linum linum :args args))
