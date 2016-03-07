(in-package :cl-user)
(defpackage :cl-lua.error
  (:use :cl :cl-lua.filepos)
  (:export
   :lua-error
   :lexer-error
   :unfinished-string-error
   :string-hex-error
   :escape-sequence-error
   :malformed-number-error
   :unfinished-long-comment-error
   :unfinished-long-string-error
   :parser-error
   :translate-error))
(in-package :cl-lua.error)

(defgeneric report (condition stream text))

(define-condition lua-error (simple-error)
  ((filepos
    :initarg :filepos
    :reader lua-error-filepos
    :type filepos)))

(defmethod report ((condition lua-error) stream text)
  (format stream
          "~A:~D ~A"
          (filepos-stream (lua-error-filepos condition))
          (filepos-linum (lua-error-filepos condition))
          text))

(define-condition lexer-error (lua-error)
  ((near
    :initarg :near
    :reader lexer-error-near
    :type string)))

(defmethod report ((condition lexer-error) stream text)
  (call-next-method condition
                    stream
                    (format nil
                            "~A near ~A"
                            text
                            (lexer-error-near condition))))

(define-condition unfinished-string-error (lexer-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition stream "unfinished string"))))

(define-condition string-hex-error (lexer-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition stream "hexadecimal digit expected"))))

(define-condition escape-sequence-error (lexer-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition stream "invalid escape sequence"))))

(define-condition malformed-number-error (lexer-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition stream "malformed number"))))

(define-condition unfinished-long-error (lexer-error)
  ((start-linum
    :initarg :start-linum
    :reader unfinished-long-start-linum
    :type integer)))

(defmethod report ((condition unfinished-long-error) stream text)
  (call-next-method condition
                    stream
                    (format nil
                            "~A (starting at line ~D)"
                            text
                            (unfinished-long-start-linum condition))))

(define-condition unfinished-long-comment-error (unfinished-long-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition stream "unfinished long comment"))))

(define-condition unfinished-long-string-error (unfinished-long-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition stream "unfinished long string"))))

(define-condition parser-error (lua-error)
  ((value
    :initarg :value
    :reader parser-error-token-value)
   (expected-tag
    :initarg :expected-tag
    :reader parser-error-expected-tag)
   (actual-tag
    :initarg :actual-tag
    :reader parser-error-actual-tag))
  (:report
   (lambda (condition stream)
     (report condition
             stream
             (format nil
                     "unexpected token ~A"
                     (cl-lua.runtime:lua-object-to-string
                      (parser-error-token-value condition)))))))

(define-condition translate-error (lua-error)
  ((text
    :initarg :text
    :reader translate-error-text
    :type string))
  (:report
   (lambda (condition stream)
     (write-line (translate-error-text condition) stream))))
