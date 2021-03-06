(defpackage :cl-lua.error
  (:use
   :cl
   :cl-lua.filepos
   :cl-lua.lua-object)
  (:export
   :lexer-error
   :unfinished-string-error
   :string-hex-error
   :escape-sequence-error
   :malformed-number-error
   :unfinished-long-comment-error
   :unfinished-long-string-error
   :syntax-error
   :break-error
   :variadic-error
   :goto-error
   :runtime-error))
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

(define-condition syntax-error (lua-error)
  ((value
    :initarg :value
    :reader syntax-error-value))
  (:report
   (lambda (condition stream)
     (report condition
             stream
             (format nil
                     "unexpected token '~A'"
                     (lua-object-to-string
                      (syntax-error-value condition)))))))

(define-condition goto-error (syntax-error)
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

(define-condition break-error (syntax-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition
             stream
             "<break> not inside a loop"))))

(define-condition variadic-error (syntax-error)
  ()
  (:report
   (lambda (condition stream)
     (report condition
             stream
             "unexpected variadic"))))

(define-condition runtime-error (lua-error)
  ((object
    :initarg :object
    :reader runtime-error-object)
   (call-stack
    :initarg :call-stack
    :reader runtime-error-call-stack))
  (:report
   (lambda (condition stream)
     (report condition
             stream
             (lua-object-to-string
              (runtime-error-object condition)))
     (format stream
             "~&stack backtrace:~%~{~A~%~}"
             (runtime-error-call-stack condition)))))
