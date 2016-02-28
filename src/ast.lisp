(in-package :cl-user)
(defpackage :cl-lua.ast
  (:use :cl)
  (:export
   :make-ast
   :ast-name
   :ast-args))
(in-package :cl-lua.ast)

(defun make-ast (name linum &rest args)
  (declare (ignore linum))
  (check-type name keyword)
  (cons name args))

(defun ast-name (ast) (car ast))
(defun ast-args (ast) (cdr ast))
