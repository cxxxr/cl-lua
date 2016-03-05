(in-package :cl-user)
(defpackage :cl-lua.util
  (:use :cl)
  (:import-from
   :alexandria
   :with-gensyms)
  (:export
   :unicode-to-utf8
   :length=1
   :string-to-bytes
   :with-accumulate
   :collect
   :lua-parse-number-decimal
   :lua-parse-number-hex
   :lua-parse-number))
(in-package :cl-lua.util)

(defun utf8-nbits (n)
  (ash (1- (ash 1 n))
       (- 8 n)))

(defun unicode-to-utf8-internal (code n)
  (let ((code-list))
    (dotimes (_ (1- n))
      (push (logior #b10000000
		    (logand #b111111 code))
	    code-list)
      (setf code (ash code -6)))
    (cons (logior (utf8-nbits n) code)
	  code-list)))

(defun unicode-to-utf8 (code)
  (if (<= 0 code #x7f)
      (list code)
      (unicode-to-utf8-internal
       code
       (cond ((<= #x80 code #x7ff) 2)
	     ((<= #x800 code #xffff) 3)
	     ((<= #x10000 code #x1fffff) 4)))))

(defun string-to-char-list (string)
  (let ((code-list)
        (n 0))
    (loop :for c :across string
          :do (dolist (code (unicode-to-utf8 (char-code c)))
                (push code code-list)
                (incf n)))
    (values (nreverse code-list) n)))

(defun string-to-bytes (string)
  (check-type string string)
  (multiple-value-bind (code-list)
      (string-to-char-list string)
    (coerce code-list '(vector (unsigned-byte 8)))))

(defun length=1 (list)
  (and (consp list)
       (null (cdr list))))

(defmacro with-accumulate (() &body body)
  (with-gensyms (gacc)
    `(let ((,gacc))
       (macrolet ((collect (x)
                    `(push ,x ,',gacc)))
         ,@body)
       (nreverse ,gacc))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-with-regex-scans (vars string-var args regexes body)
    (when regexes
      `(multiple-value-bind ,vars
	   (ppcre:scan ,(car regexes) ,string-var ,@args)
	 (if (or ,@vars)
             (progn ,@body)
             ,(gen-with-regex-scans vars
                                    string-var
                                    args
                                    (cdr regexes)
                                    body))))))

(defmacro with-regex-scans (((&rest vars) (string-var &rest args)
                             &rest regexes)
                            &body body)
  (gen-with-regex-scans vars string-var args regexes body))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-with-regex-groups (n vars gstring gstart-groups gend-groups body)
    (if (null vars)
        `(progn ,@body)
        `(let ((,(car vars)
                 (when (aref ,gstart-groups ,n)
                   (subseq ,gstring
                           (aref ,gstart-groups ,n)
                           (aref ,gend-groups ,n)))))
           ,(gen-with-regex-groups (1+ n)
                                   (cdr vars)
                                   gstring
                                   gstart-groups
                                   gend-groups
                                   body)))))

(defmacro with-regex-groups ((vars string start-groups end-groups) &body body)
  (with-gensyms (gstring gstart-groups gend-groups)
    `(let ((,gstring ,string)
           (,gstart-groups ,start-groups)
           (,gend-groups ,end-groups))
       ,(gen-with-regex-groups 0
                               vars
                               gstring
                               gstart-groups
                               gend-groups
                               body))))

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
