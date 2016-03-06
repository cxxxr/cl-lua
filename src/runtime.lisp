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
   :lua-object-to-string
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
   :lua-index))
(in-package :cl-lua.runtime)

(defvar +lua-nil+ (make-symbol "NIL"))
(defvar +lua-false+ (make-symbol "FALSE"))
(defvar +lua-true+ (make-symbol "TRUE"))
(defvar +lua-rest-symbol+ (make-symbol "..."))
(defvar +lua-env-name+ (make-symbol "ENV"))

(defstruct (lua-table (:constructor make-lua-table-internal))
  (hash-table (make-hash-table) :type hash-table :read-only t)
  (sequence-length 0 :type integer))

(defun make-lua-table (linum &key sequence pairs)
  (let ((table (make-hash-table :test #'equalp))
        (max-index (length sequence)))
    (loop :for (k v) :in pairs
          :when (lua-eq linum k 1)
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

(defun lua-string-to-string (lua-string)
  (babel:octets-to-string lua-string))

(defun lua-object-to-string (x)
  (typecase x
    (lua-string
     (prin1-to-string (lua-string-to-string x)))
    (t
     (princ-to-string x))))

(defun lua-unm (linum x)
  (declare (ignore linum x)))

(defun lua-not (linum x)
  (declare (ignore linum x)))

(defun lua-len (linum x)
  (declare (ignore linum x)))

(defun lua-bnot (linum x)
  (declare (ignore linum x)))

(defun lua-add (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-sub (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-mul (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-div (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-idiv (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-pow (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-mod (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-band (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-bxor (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-bor (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-shr (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-shl (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-concat (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-lt (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-le (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-gt (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-ge (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-eq (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-ne (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-and (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-or (linum x y)
  (declare (ignore linum x y))
  )

(defun lua-index (linum table key)
  (declare (ignore linum table key))
  )

(defun (setf lua-index) (new-value linum table key)
  (declare (ignore linum new-value table key)))
