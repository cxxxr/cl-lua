(defpackage :cl-lua.lua-object
  (:use :cl :cl-lua.util)
  (:export
   :make-lua-table
   :lua-table
   :lua-table-p
   :lua-table-metatable
   :lua-table-get
   :lua-table-put
   :lua-table-put-if-exists
   :lua-table-len
   :lua-parse-number
   :lua-string
   :make-empty-lua-string
   :string-to-lua-string
   :lua-object-to-string
   :lua-string-to-string
   :lua-string-to-number))
(in-package :cl-lua.lua-object)

(defstruct (lua-table (:constructor make-lua-table-internal))
  (hash-table (make-hash-table :test 'eql) :type hash-table :read-only t)
  (sequence-length 0 :type fixnum)
  (metatable nil :type (or null lua-table)))

(defun make-lua-table ()
  (make-lua-table-internal))

(defun lua-table-get (table key default)
  (declare (lua-table table))
  (gethash key (lua-table-hash-table table) default))

(defun lua-table-put (table key value)
  (declare (lua-table table))
  (let ((index (if (integerp key)
                   (when (<= 0 key) key)
                   (and (numberp key)
                        (<= 0 key)
                        (multiple-value-bind (a b)
                            (floor key)
                          (when (zerop b) a)))))
        (ht (lua-table-hash-table table)))
    (when (and index
               (or (zerop index)
                   (gethash (1- index) ht)))
      (do ((i index (1+ i)))
          ((null (gethash i ht)))
        (incf (lua-table-sequence-length table)))))
  (setf (gethash key (lua-table-hash-table table))
        value))

(defun lua-table-put-if-exists (table key value)
  (cond ((gethash key (lua-table-hash-table table))
         (lua-table-put table key value)
         value)
        (t nil)))

(defun lua-table-len (table)
  (declare (lua-table table))
  (lua-table-sequence-length table))

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

(defvar *lua-string-cache-table* (make-hash-table :test #'equal))

(defun make-empty-lua-string ()
  (string-to-lua-string ""))

(defun string-to-lua-string (string)
  (check-type string string)
  (or (gethash string *lua-string-cache-table*)
      (setf (gethash string *lua-string-cache-table*)
            (babel:string-to-octets string))))

(set-dispatch-macro-character
 #\# #\L
 #'(lambda (stream c1 c2)
     (declare (ignore c1 c2))
     (let ((string (read stream t nil t)))
       (check-type string string)
       (string-to-lua-string string))))

(defun lua-string-to-string (lua-string)
  (babel:octets-to-string lua-string))

(defun lua-object-to-string (x)
  (typecase x
    (lua-string
     (princ-to-string (lua-string-to-string x)))
    (t
     (princ-to-string x))))

(defun lua-string-to-number (lua-string)
  (lua-parse-number
   (string-trim '(#\space #\tab)
                (lua-string-to-string lua-string))))
