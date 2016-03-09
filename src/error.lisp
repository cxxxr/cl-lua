(in-package :cl-user)
(defpackage :cl-lua.error
  (:use :cl :cl-lua.filepos)
  (:import-from
   :cl-lua.lua-object
   :lua-object-to-string)
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
   :break-error
   :variadic-error
   :goto-error))
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
          (filepos-stream-info (lua-error-filepos condition))
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
                     (lua-object-to-string
                      (parser-error-token-value condition)))))))

(define-condition translate-error (lua-error)
  ())

(define-condition goto-error (translate-error)
  ((name
    :initarg :name
    :reader goto-error-name
    :type string))
  (:report
   (lambda (condition stream)
     (report condition
             stream
             (format nil "no visible label ~A for <goto>"
                     (goto-error-name condition))))))

(define-condition break-error (translate-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition
             stream
             "<break> not inside a loop"))))

(define-condition variadic-error (translate-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition
             stream
             "unexpected variadic"))))

(define-condition runtime-error (lua-error)
  ())
