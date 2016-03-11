(in-package :cl-user)
(defpackage :cl-lua.runtime
  (:use
   :cl
   :cl-lua.util
   :cl-lua.lua-object)
  (:import-from
   :alexandria
   :with-gensyms)
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

(defun lua-bool (x)
  (if x +lua-true+ +lua-false+))

(defun lua-not (filepos x)
  (declare (ignore filepos))
  (if (lua-false-p x)
      +lua-true+
      +lua-false+))

(defun lua-and (filepos x y)
  (declare (ignore filepos))
  (if (lua-false-p x)
      x
      y))

(defun lua-or (filepos x y)
  (declare (ignore filepos))
  (if (lua-false-p x)
      y
      x))

(defun to-number (x)
  (typecase x
    (lua-string
     (lua-string-to-number x))
    (number
     x)))

(defun to-integer (x)
  (typecase x
    (lua-string
     (to-integer (lua-string-to-number x)))
    (float
     (multiple-value-bind (a b) (floor x)
       (when (zerop b)
         a)))
    (integer
     x)))

(defun to-string (x)
  (typecase x
    (lua-string
     x)
    (number
     (string-to-lua-string (prin1-to-string x)))))

(defvar *metatable-table* (make-hash-table))

(defun get-metamethod (x name)
  (let ((table (gethash *metatable-table* x)))
    (gethash name table)))

(defmacro call-metamethod-between (name &rest args)
  (with-gensyms (gresult)
    (labels ((f (args)
               (if (null args)
                   nil
                   (let ((x (car args)))
                     `(let ((,gresult (get-metamethod ,x ,name)))
                        (if (functionp ,gresult)
                            (funcall ,gresult ,@args)
                            ,(f (cdr args))))))))
      (f args))))

(defmacro call-metamethod (funcall name &rest args)
  (with-gensyms (gresult)
    `(let ((,gresult (get-metamethod ,name ,(car args))))
       (if (functionp ,gresult)
           (,funcall ,gresult ,@args)))))

(defmacro op1 (filepos cast-function-name x op metamethod-name)
  `(let ((,x (,cast-function-name ,x)))
     (cond (x (,op ,x))
           ((call-metamethod-between ,metamethod-name ,x))
           (t (error 'runtime-error :filepos ,filepos)))))

(defmacro op2 (filepos cast-function-name x y op metamethod-name)
  `(let ((,x (,cast-function-name ,x))
         (,y (,cast-function-name ,y)))
     (cond ((and ,x ,y) (,op ,x ,y))
           ((call-metamethod-between ,metamethod-name ,x ,y))
           (t (error 'runtime-error :filepos ,filepos)))))

(defun lua-add (filepos x y)
  (op2 filepos to-number x y + :__add))

(defun lua-sub (filepos x y)
  (op2 filepos to-number x y - :__sub))

(defun lua-mul (filepos x y)
  (op2 filepos to-number x y * :__mul))

(defun lua-div (filepos x y)
  (op2 filepos to-number x y (lambda (a b) (float (/ a b))) :__div))

(defun lua-mod (filepos x y)
  (op2 filepos to-number x y mod :__mod))

(defun lua-pow (filepos x y)
  (op2 filepos to-number x y expt :__pow))

(defun lua-unm (filepos x)
  (op1 filepos to-number x - :__unm))

(defun lua-idiv (filepos x y)
  (op2 filepos to-number x y (lambda (a b) (values (floor a b))) :__idiv))

(defun lua-band (filepos x y)
  (op2 filepos to-integer x y logand :__band))

(defun lua-bor (filepos x y)
  (op2 filepos to-integer x y logior :__bor))

(defun lua-bxor (filepos x y)
  (op2 filepos to-integer x y logxor :__bxor))

(defun lua-bnot (filepos x)
  (op1 filepos to-integer x lognot :__bnot))

(defun lua-shl (filepos x y)
  (op2 filepos to-integer x y (lambda (a b) (ash a (- b))) :__shl))

(defun lua-shr (filepos x y)
  (op2 filepos to-integer x y (lambda (a b) (ash a b)) :__shr))

(defun lua-concat (filepos x y)
  (op2 filepos to-string x y (lambda (a b) (concatenate 'lua-string a b))
       :__concat))

(defun lua-len (filepos x)
  (typecase x
    (lua-string
     (length x))
    (otherwise
     (or (call-metamethod-between :__len x)
         (error 'runtime-error :filepos filepos)))))

(defun lua-eq (filepos x y)
  (tagbody
     (return-from lua-eq
       (typecase x
         (number
          (typecase y
            (number
             (lua-bool (= x y)))
            (otherwise
             (go :fail))))
         (lua-string
          (typecase y
            (lua-string
             (let ((len (length x)))
               (if (= len (length y))
                   (dotimes (i len +lua-true+)
                     (unless (= (aref x i) (aref y i))
                       (return-from lua-eq +lua-false+)))
                   +lua-false+)))
            (otherwise
             (go :fail))))
         (otherwise
          (typecase y
            (lua-string +lua-false+)
            (number +lua-false+)
            (otherwise
             (if (eq x y)
                 +lua-true+
                 (go :fail)))))))
   :fail
     (return-from lua-eq
       (or (lua-bool (call-metamethod-between :__eq x y))
           (error 'runtime-error :filepos filepos)))))

(defun lua-ne (filepos x y)
  (lua-not filepos (lua-eq filepos x y)))

(defmacro cmp ((x y op1 op2) &body fail-body)
  `(typecase ,x
     (number
      (typecase ,y
        (number
         (lua-bool (,op1 ,x ,y)))
        (otherwise
         ,@fail-body)))
     (lua-string
      (typecase ,y
        (lua-string
         (let ((len1 (length ,x))
               (len2 (length ,y)))
           (dotimes (i (min len1 len2)
                       (lua-bool (,op1 len1 len2)))
             (let ((e1 (aref ,x i))
                   (e2 (aref ,y i)))
               (cond ((,op1 e1 e2) (return +lua-true+))
                     ((,op2 e1 e2) (return +lua-false+)))))))
        (otherwise
         ,@fail-body)))))

(defun lua-lt (filepos x y)
  (cmp (x y < >)
    (or (call-metamethod-between :__lt x y)
        (error 'runtime-error :filepos filepos))))

(defun lua-le (filepos x y)
  (cmp (x y <= >)
    (or (call-metamethod-between :__le x y)
        (lua-not filepos (call-metamethod-between :__lt y x))
        (error 'runtime-error :filepos filepos))))

(defun lua-gt (filepos x y)
  (lua-lt filepos y x))

(defun lua-ge (filepos x y)
  (lua-le filepos y x))

(defun lua-index (filepos table key)
  (labels ((metamethod (table key)
             (let ((x (get-metamethod table :__index)))
               (typecase x
                 (function
                  (funcall x table key))
                 (lua-table
                  (lua-index filepos x key))))))
    (typecase table
      (lua-table
       (or (gethash key (lua-table-hash-table table))
           (metamethod table key)
           +lua-nil+))
      (otherwise
       (or (metamethod table key)
           (error 'runtime-error :filepos filepos))))))

(defun (setf lua-index) (value filepos table key)
  (labels ((metamethod (table key value)
             (let ((x (get-metamethod table :__newindex)))
               (typecase x
                 (function
                  (funcall x table key value))
                 (lua-table
                  (setf (lua-index filepos x key) value))))))
    (typecase table
      (lua-table
       (cond ((gethash key (lua-table-hash-table table))
              (setf (gethash key (lua-table-hash-table table)) value))
             ((metamethod table key value))
             (t (setf (gethash key (lua-table-hash-table table)) value))))
      (otherwise
       (or (metamethod table key value)
           (error 'runtime-error :filepos filepos))))))

(defun lua-call (filepos fun &rest args)
  (typecase fun
    (function
     (funcall fun args))
    (otherwise
     (or (call-metamethod apply :__call fun args)
         (error 'runtime-error :filepos filepos)))))
