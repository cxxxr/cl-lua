(in-package :cl-user)
(defpackage :cl-lua.runtime
  (:use
   :cl
   :cl-lua.util
   :cl-lua.lua-object
   :cl-lua.error)
  (:import-from
   :alexandria
   :with-gensyms
   :once-only)
  (:export
   :+lua-nil+
   :+lua-false+
   :+lua-true+
   :+lua-rest-symbol+
   :+lua-env-name+
   :make-init-env
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
   :lua-call
   :with-runtime))
(in-package :cl-lua.runtime)

(defvar +lua-nil+ (make-symbol "NIL"))
(defvar +lua-false+ (make-symbol "FALSE"))
(defvar +lua-true+ (make-symbol "TRUE"))
(defvar +lua-rest-symbol+ (make-symbol "..."))
(defvar +lua-env-name+ (make-symbol "ENV"))

(defun make-init-env ()
  (let ((table (make-lua-table)))
    table))

(defun runtime-error (filepos &optional string &rest args)
  (error 'runtime-error
         :filepos filepos
         :object (apply #'format nil string args)
         :call-stack nil))

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

(defun to-integer-p (x)
  (typecase x
    (lua-string
     (to-integer-p (lua-string-to-number x)))
    (float
     (multiple-value-bind (a b) (floor x)
       (declare (ignore a))
       (zerop b)))
    (integer
     t)))

(defun to-string (x)
  (typecase x
    (lua-string
     x)
    (number
     (string-to-lua-string (prin1-to-string x)))))

(defvar *metatable-table* (make-hash-table))

(defun get-metamethod (x name)
  (let ((table (if (lua-table-p x)
                   (lua-table-metatable x)
                   (gethash x *metatable-table*))))
    (when table
      (gethash name table))))

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

(defun lua-type-of (x)
  (cond ((typep x 'lua-string) "string")
        ((typep x 'integer)    "integer")
        ((typep x 'number)     "number")
        ((typep x 'lua-table)  "table")
        ((typep x 'function)   (prin1-to-string x))
        ((eq x +lua-nil+)      "nil")
        ((eq x +lua-true+)     "true")
        ((eq x +lua-false+)    "false")
        (t
         (warn "unknown object: ~A" x)
         (prin1-to-string x))))

(defmacro arith (filepos x y op metamethod-name)
  (check-type x symbol)
  (check-type y symbol)
  (with-gensyms (gx gy)
    `(let ((,gx (to-number ,x))
           (,gy (to-number ,y)))
       (cond ((and ,gx ,gy) (,op ,gx ,gy))
             ((call-metamethod-between ,metamethod-name ,x ,y))
             (t
              (runtime-error ,filepos
                             "attempt to perform arithmetic on a ~A value"
                             (lua-type-of (if (null ,gx) ,x ,y))))))))

(defmacro arith-unary (filepos x op metamethod-name)
  (check-type x symbol)
  (with-gensyms (gx)
    `(let ((,gx (to-number ,x)))
       (cond ((and ,gx) (,op ,gx))
             ((call-metamethod-between ,metamethod-name ,x))
             (t
              (runtime-error ,filepos
                             "attempt to perform arithmetic on a ~A value"
                             (lua-type-of ,x)))))))

(defmacro arith-bit (filepos x y op metamethod-name)
  (check-type x symbol)
  (check-type y symbol)
  (with-gensyms (gx gy)
    `(let ((,gx (to-integer ,x))
           (,gy (to-integer ,y)))
       (cond ((and ,gx ,gy) (,op ,gx ,gy))
             ((call-metamethod-between ,metamethod-name ,x ,y))
             ((or (not (to-integer-p ,x)) (not (to-integer-p ,y)))
              (runtime-error ,filepos
                             "number has no integer representation"))
             (t
              (runtime-error
               ,filepos
               "attempt to perform between operation on a ~A value"
               (lua-type-of (if (null ,gx) ,x ,y))))))))

(defmacro arith-bit-unary (filepos x op metamethod-name)
  (check-type x symbol)
  (with-gensyms (gx)
    `(let ((,gx (to-integer ,x)))
       (cond (,gx (,op ,gx))
             ((call-metamethod-between ,metamethod-name ,x))
             ((not (to-integer-p ,x))
              (runtime-error ,filepos
                             "number has no integer representation"))
             (t
              (runtime-error
               ,filepos
               "attempt to perform between operation on a ~A value"
               (lua-type-of ,x)))))))

(defun lua-add (filepos x y)
  (arith filepos x y + :__add))

(defun lua-sub (filepos x y)
  (arith filepos x y - :__sub))

(defun lua-mul (filepos x y)
  (arith filepos x y * :__mul))

(defun lua-div (filepos x y)
  (arith filepos x y (lambda (a b) (float (/ a b))) :__div))

(defun lua-mod (filepos x y)
  (arith filepos x y mod :__mod))

(defun lua-pow (filepos x y)
  (arith filepos x y expt :__pow))

(defun lua-unm (filepos x)
  (arith-unary filepos x - :__unm))

(defun lua-idiv (filepos x y)
  (arith filepos x y (lambda (a b) (values (floor a b))) :__idiv))

(defun lua-band (filepos x y)
  (arith-bit filepos x y logand :__band))

(defun lua-bor (filepos x y)
  (arith-bit filepos x y logior :__bor))

(defun lua-bxor (filepos x y)
  (arith-bit filepos x y logxor :__bxor))

(defun lua-bnot (filepos x)
  (arith-bit-unary filepos x lognot :__bnot))

(defun lua-shl (filepos x y)
  (arith-bit filepos x y (lambda (a b) (ash a (- b))) :__shl))

(defun lua-shr (filepos x y)
  (arith-bit filepos x y (lambda (a b) (ash a b)) :__shr))

(defun lua-concat (filepos x y)
  (let ((x1 (to-string x))
        (y1 (to-string y)))
    (cond ((and x1 y1) (concatenate 'lua-string x1 y1))
          ((call-metamethod-between :__concat x y))
          (t (runtime-error filepos
                            "attempt to concatenate a ~A value"
                            (if (null x1)
                                (lua-type-of x)
                                (lua-type-of y)))))))

(defun lua-len (filepos x)
  (typecase x
    (lua-string
     (length x))
    (lua-table
     (or (call-metamethod-between :__len x)
         (lua-table-len x)))
    (otherwise
     (or (call-metamethod-between :__len x)
         (runtime-error filepos
                        "attempt to get length of a ~A value"
                        x)))))

(defun lua-eq (filepos x y)
  (declare (ignore filepos))
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
       (lua-bool
        (or (call-metamethod-between :__eq x y)
            +lua-false+)))))

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

(defun cmp-error (filepos x y)
  (runtime-error filepos
                 "attempt to compare ~A with ~A"
                 (lua-type-of x)
                 (lua-type-of y)))

(defun lua-lt (filepos x y)
  (cmp (x y < >)
    (or (call-metamethod-between :__lt x y)
        (cmp-error filepos x y))))

(defun lua-le (filepos x y)
  (cmp (x y <= >)
    (or (call-metamethod-between :__le x y)
        (lua-not filepos (call-metamethod-between :__lt y x))
        (cmp-error filepos x y))))

(defun lua-gt (filepos x y)
  (lua-lt filepos y x))

(defun lua-ge (filepos x y)
  (lua-le filepos y x))

(defun index-error (filepos table)
  (runtime-error filepos
                 "attempt to index a ~A value"
                 (lua-type-of table)))

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
       (or (lua-table-get table key)
           (metamethod table key)
           +lua-nil+))
      (otherwise
       (or (metamethod table key)
           (index-error filepos table))))))

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
       (or (lua-table-put-if-exists table key value)
           (metamethod table key value)
           (lua-table-put table key value)))
      (otherwise
       (or (metamethod table key value)
           (index-error filepos table))))))

(defmacro lua-call (filepos fun &rest args)
  (once-only (fun)
    `(typecase ,fun
       (function
        (multiple-value-call ,fun ,@args))
       (otherwise
        (or (call-metamethod multiple-value-call :__call ,fun ,@args)
            (runtime-error ,filepos
                           "attempt to call a ~A value"
                           ,fun))))))

(defmacro with-runtime (() &body body)
  `(let ((,+lua-env-name+ (make-init-env)))
     (declare (ignorable ,+lua-env-name+))
     ,@body))
