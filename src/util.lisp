(in-package :cl-user)
(defpackage :cl-lua.util
  (:use :cl)
  (:import-from
   :alexandria
   :with-gensyms)
  (:export
   :unicode-to-utf8
   :length=1
   :with-accumulate
   :collect
   :with-regex-scans
   :with-regex-groups))
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
