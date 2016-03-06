(in-package :cl-user)
(defpackage :cl-lua.lua-object
  (:use :cl :cl-lua.util)
  (:export
   :lua-parse-number-decimal
   :lua-parse-number-hex
   :lua-parse-number
   :lua-string
   :make-lua-string
   :string-to-lua-string))
(in-package :cl-lua.lua-object)

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
