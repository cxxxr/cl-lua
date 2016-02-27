(in-package :cl-user)
(defpackage :cl-lua.error
  (:use :cl)
  (:export
   :lua-error
   :lexer-error
   :parser-error))
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
  ((expected-tag
    :initarg :expected-tag
    :reader parser-error-expected-tag)
   (actual-tag
    :initarg :actual-tag
    :reader parser-error-actual-tag)
   (linum
    :initarg :linum
    :reader parser-error-linum
    :type integer)))
