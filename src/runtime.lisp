(in-package :cl-user)
(defpackage :cl-lua.runtime
  (:use :cl :cl-lua.util)
  (:export
   :+lua-nil+
   :+lua-false+
   :+lua-true+
   :+lua-rest-symbol+
   :+lua-env-name+
   :lua-false-p
   :lua-not
   :lua-and
   :lua-or
   :lua-add
   :lua-sub
   :lua-mul
   :lua-div
   :lua-mod
   :lua-pow
   :lua-unm
   :lua-idiv
   :lua-band
   :lua-bor
   :lua-bxor
   :lua-bnot
   :lua-shl
   :lua-shr
   :lua-concat
   :lua-len
   :lua-eq
   :lua-ne
   :lua-lt
   :lua-le
   :lua-gt
   :lua-ge
   :lua-index
   :lua-call))
(in-package :cl-lua.runtime)

(defvar +lua-nil+ (make-symbol "NIL"))
(defvar +lua-false+ (make-symbol "FALSE"))
(defvar +lua-true+ (make-symbol "TRUE"))
(defvar +lua-rest-symbol+ (make-symbol "..."))
(defvar +lua-env-name+ (make-symbol "ENV"))

(defun lua-false-p (x)
  (or (eq x +lua-nil+)
      (eq x +lua-false+)))

(defun lua-not (filepos x)
  (declare (ignore filepos)))

(defun lua-and (filepos x y)
  (declare (ignore filepos)))

(defun lua-or (filepos x y)
  (declare (ignore filepos)))

(defun lua-add (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-sub (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-mul (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-div (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-mod (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-pow (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-unm (filepos x)
  (declare (ignore filepos x)))

(defun lua-idiv (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-band (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-bor (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-bxor (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-bnot (filepos x)
  (declare (ignore filepos x)))

(defun lua-shl (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-shr (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-concat (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-len (filepos x)
  (declare (ignore filepos x)))

(defun lua-eq (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-ne (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-lt (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-le (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-gt (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-ge (filepos x y)
  (declare (ignore filepos x y)))

(defun lua-index (filepos table key)
  (declare (ignore filepos table key)))

(defun (setf lua-index) (new-value filepos table key)
  (declare (ignore filepos new-value table key)))

(defun lua-call (filepos fun &rest args)
  (declare (ignore filepos fun args)))
