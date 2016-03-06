(in-package :cl-user)
(defpackage :cl-lua.error
  (:use :cl :cl-lua.token)
  (:export
   :lua-error
   :lexer-error
   :unfinished-string-error
   :string-hex-error
   :escape-sequence-error
   :malformed-number-error
   :parser-error
   :translate-error))
(in-package :cl-lua.error)

(defgeneric report (condition stream text))

(define-condition lua-error (simple-error)
  ((linum
    :initarg :linum
    :reader lua-error-linum)
   (stream
    :initarg :stream
    :reader lua-error-stream)))

(defmethod report ((condition lua-error) stream text)
  (format stream
          "~A:~D ~A"
          (lua-error-stream condition)
          (lua-error-linum condition)
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
                     (parser-error-token-value condition))))))

(define-condition translate-error (lua-error)
  ((text
    :initarg :text
    :reader translate-error-text
    :type string))
  (:report
   (lambda (condition stream)
     (write-line (translate-error-text condition) stream))))
