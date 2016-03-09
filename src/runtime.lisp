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
   :lua-unm
   :lua-not
   :lua-len
   :lua-bnot
   :lua-add
   :lua-sub
   :lua-mul
   :lua-div
   :lua-idiv
   :lua-pow
   :lua-mod
   :lua-band
   :lua-bxor
   :lua-bor
   :lua-shl
   :lua-shr
   :lua-concat
   :lua-lt
   :lua-le
   :lua-gt
   :lua-ge
   :lua-eq
   :lua-ne
   :lua-and
   :lua-or
   :lua-index
   :call-function))
(in-package :cl-lua.runtime)

(defvar +lua-nil+ (make-symbol "NIL"))
(defvar +lua-false+ (make-symbol "FALSE"))
(defvar +lua-true+ (make-symbol "TRUE"))
(defvar +lua-rest-symbol+ (make-symbol "..."))
(defvar +lua-env-name+ (make-symbol "ENV"))

(defun lua-false-p (x)
  (or (eq x +lua-nil+)
      (eq x +lua-false+)))

(defun lua-unm (filepos x)
  (declare (ignore filepos x))
  )

(defun lua-not (filepos x)
  (declare (ignore filepos x)))

(defun lua-len (filepos x)
  (declare (ignore filepos x)))

(defun lua-bnot (filepos x)
  (declare (ignore filepos x)))

(defun lua-add (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-sub (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-mul (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-div (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-idiv (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-pow (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-mod (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-band (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-bxor (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-bor (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-shr (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-shl (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-concat (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-lt (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-le (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-gt (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-ge (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-eq (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-ne (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-and (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-or (filepos x y)
  (declare (ignore filepos x y))
  )

(defun lua-index (filepos table key)
  (declare (ignore filepos table key))
  )

(defun (setf lua-index) (new-value filepos table key)
  (declare (ignore filepos new-value table key)))

(defun call-function (filepos fun &rest args)
  (declare (ignore filepos fun args))
  )
