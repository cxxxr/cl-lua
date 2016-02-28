(in-package :cl-user)
(defpackage :cl-lua.ast
  (:use :cl)
  (:export
   :make-ast
   :ast-name
   :ast-args))
(in-package :cl-lua.ast)

(defstruct (ast (:constructor make-ast-internal))
  name
  linum
  args)

(defmethod print-object ((ast ast) stream)
  (format stream "(~S ~{~S~^ ~})" (ast-name ast) (ast-args ast)))

(defun make-ast (name linum &rest args)
  (check-type name keyword)
  (unless (eq name :void)
    (check-type linum integer))
  (make-ast-internal :name name :linum linum :args args))
