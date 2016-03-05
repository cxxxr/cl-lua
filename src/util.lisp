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
   :lua-parse-number-decimal))
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
      (return-from lua-parse-number-dec
        (multiple-value-bind (value index)
            (read-from-string string
                              t
                              nil
                              :start res-start
                              :end res-end)
          (values value index))))))
