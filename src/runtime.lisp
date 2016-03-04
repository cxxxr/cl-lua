(in-package :cl-user)
(defpackage :cl-lua.runtime
  (:use :cl)
  (:export
   :make-lua-table
   :+lua-nil+
   :+lua-false+
   :+lua-true+
   :+lua-rest-symbol+
   :+lua-env-name+
   :lua-minus
   :lua-not
   :lua-len
   :lua-lognot-unary
   :lua-add
   :lua-sub
   :lua-mul
   :lua-div
   :lua-ndiv
   :lua-pow
   :lua-mod
   :lua-logand
   :lua-lognot
   :lua-logior
   :lua-rshift
   :lua-lshift
   :lua-concat
   :lua-lt
   :lua-le
   :lua-gt
   :lua-ge
   :lua-eq
   :lua-ne
   :lua-and
   :lua-or
   :lua-get-table))
(in-package :cl-lua.runtime)

(defvar +lua-nil+ (make-symbol "NIL"))
(defvar +lua-false+ (make-symbol "FALSE"))
(defvar +lua-true+ (make-symbol "TRUE"))
(defvar +lua-rest-symbol+ (make-symbol "..."))
(defvar +lua-env-name+ (make-symbol "ENV"))

(defstruct (lua-table (:constructor make-lua-table-internal))
  (hash-table (make-hash-table) :type hash-table :read-only t)
  (sequence-length 0 :type integer))

(defun make-lua-table (&key sequence pairs)
  (let ((table (make-hash-table :test #'equalp))
        (max-index (length sequence)))
    (loop :for (k v) :in pairs
          :when (lua-eq k 1)
            :do (when (< max-index k)
                  (setf max-index k))
          :do (setf (gethash k table) v))
    (loop :for elt :across sequence
          :for i :from 1
          :do (setf (gethash i table) elt))
    (make-lua-table-internal :hash-table table
                             :sequence-length max-index)))

(defun lua-minus (x)
  (declare (ignore x)))

(defun lua-not (x)
  (declare (ignore x)))

(defun lua-len (x)
  (declare (ignore x)))

(defun lua-lognot-unary (x)
  (declare (ignore x)))

(defun lua-add (x y)
  (declare (ignore x y))
  )

(defun lua-sub (x y)
  (declare (ignore x y))
  )

(defun lua-mul (x y)
  (declare (ignore x y))
  )

(defun lua-div (x y)
  (declare (ignore x y))
  )

(defun lua-ndiv (x y)
  (declare (ignore x y))
  )

(defun lua-pow (x y)
  (declare (ignore x y))
  )

(defun lua-mod (x y)
  (declare (ignore x y))
  )

(defun lua-logand (x y)
  (declare (ignore x y))
  )

(defun lua-lognot (x y)
  (declare (ignore x y))
  )

(defun lua-logior (x y)
  (declare (ignore x y))
  )

(defun lua-rshift (x y)
  (declare (ignore x y))
  )

(defun lua-lshift (x y)
  (declare (ignore x y))
  )

(defun lua-concat (x y)
  (declare (ignore x y))
  )

(defun lua-lt (x y)
  (declare (ignore x y))
  )

(defun lua-le (x y)
  (declare (ignore x y))
  )

(defun lua-gt (x y)
  (declare (ignore x y))
  )

(defun lua-ge (x y)
  (declare (ignore x y))
  )

(defun lua-eq (x y)
  (declare (ignore x y))
  )

(defun lua-ne (x y)
  (declare (ignore x y))
  )

(defun lua-and (x y)
  (declare (ignore x y))
  )

(defun lua-or (x y)
  (declare (ignore x y))
  )

(defun lua-get-table (table key)
  (declare (ignore table key))
  )

(defun (setf lua-get-table) (new-value table key)
  (declare (ignore new-value table key)))
