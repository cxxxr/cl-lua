(in-package :cl-user)
(defpackage :cl-lua.error
  (:use :cl :cl-lua.token)
  (:export
   :lua-error
   :lexer-error
   :parser-error
   :translate-error))
(in-package :cl-lua.error)

(define-condition lua-error (simple-error)
  ())

(define-condition lexer-error (lua-error)
  ((text :initarg :text :reader lexer-error-text :type string)
   (near :initarg :near :reader lexer-error-near :type string)
   (linum :initarg :linum :reader lexer-error-linum :type integer)
   (stream :initarg :stream :reader lexer-error-stream :type stream))
  (:report
   (lambda (condition stream)
     (format stream
             "~A:~D: ~A near ~A"
             (lexer-error-stream condition)
             (lexer-error-linum condition)
             (lexer-error-text condition)
             (lexer-error-near condition)))))

(define-condition parser-error (lua-error)
  ((token
    :initarg :token
    :reader parser-error-token
    :type token)
   (expected-tag
    :initarg :expected-tag
    :reader parser-error-expected-tag)
   (actual-tag
    :initarg :actual-tag
    :reader parser-error-actual-tag))
  (:report
   (lambda (condition stream)
     (format stream
             "expected-tag = '~A'~@
              actual-tag = '~A'~@
              token = ~A"
             (parser-error-expected-tag condition)
             (parser-error-actual-tag condition)
             (parser-error-token condition)))))

(define-condition translate-error (lua-error)
  ((text
    :initarg :text
    :reader translate-error-text
    :type string))
  (:report
   (lambda (condition stream)
     (write-line (translate-error-text condition) stream))))
