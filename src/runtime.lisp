(in-package :cl-user)
(defpackage :cl-lua.runtime
  (:use :cl :cl-lua.util)
  (:export
   :+lua-nil+
   :+lua-false+
   :+lua-true+
   :+lua-rest-symbol+
   :+lua-env-name+
   :make-lua-table
   :lua-parse-number
   :lua-string
   :make-lua-string
   :string-to-lua-string
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

(defun lua-parse-number-decimal (string
                                 &key
                                   (start 0)
                                   (end (length string))
                                   junk-allowed)
  (with-regex-scans ((res-start res-end)
                     (string :start start :end end)
                     "^[0-9]+\\.(?:[0-9]+(?:[eE][+\\-]?[0-9]+)?)?"
                     "^\\.[0-9]+(?:[eE][+\\-]?[0-9]+)?"
                     "^[0-9]+[eE][+\\-]?[0-9]+"
                     "^[0-9]+")
    (when (and (= start res-start)
               (or junk-allowed (= end res-end)))
      (multiple-value-bind (value index)
          (read-from-string string
                            t
                            nil
                            :start res-start
                            :end res-end)
        (values value index)))))

(defun lua-parse-number-hex (string
                             &key
                               (start 0)
                               (end (length string))
                               junk-allowed)
  (multiple-value-bind (res-start res-end start-groups end-groups)
      (ppcre:scan
       "^0[xX]([a-fA-F0-9]+)?(?:\\.([a-fA-F0-9]+))?(?:[pP]([+\\-]?[0-9]+))?"
       string
       :start start
       :end end)
    (when (and (= start res-start)
               (or junk-allowed (= end res-end)))
      (with-regex-groups ((int-str float-str exp-str)
                          string
                          start-groups
                          end-groups)
        (unless (and (null int-str)
                     (null float-str)
                     (null exp-str))
          (values (if (and int-str (null float-str) (null exp-str))
                      (parse-integer int-str :radix 16)
                      (float (* (+ (if int-str
                                       (parse-integer int-str :radix 16)
                                       0)
                                   (if float-str
                                       (/ (parse-integer float-str :radix 16)
                                          (expt 16 (length float-str)))
                                       0))
                                (if exp-str
                                    (float (expt 2 (parse-integer exp-str)))
                                    1))))
                  res-end))))))

(defun lua-parse-number (string
                         &key
                           (start 0)
                           (end (length string))
                           junk-allowed)
  (funcall (if (ppcre:scan "^0[xX]" string :start start)
               #'lua-parse-number-hex
               #'lua-parse-number-decimal)
           string
           :start start
           :end end
           :junk-allowed junk-allowed))

(deftype lua-string (&optional n)
  `(simple-array (unsigned-byte 8) (,n)))

(defun make-lua-string (n)
  (make-array n
              :element-type '(unsigned-byte 8)
              :initial-element 0))

(defun string-to-lua-string (string)
  (check-type string string)
  (babel:string-to-octets string))

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
