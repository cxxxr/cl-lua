(in-package :cl-user)
(defpackage :cl-lua.baselib
  (:use :cl :cl-lua.libutil :cl-lua.runtime))
(in-package :cl-lua.baselib)

(define-lua-function "print" (&rest args)
  (do ((rest args (cdr rest)))
      ((null rest))
    (princ (tostring (car rest)))
    (if (cdr rest)
        (princ #\tab)
        (terpri)))
  +lua-nil+)

(defun tostring (v)
  (let ((f (get-metamethod v :__tostring)))
    (if f
        (funcall f v)
        (write-to-string v :case :downcase :escape nil :readably nil))))

(define-lua-function "tostring" (v)
  (tostring v))
