(defpackage :cl-lua.baselib
  (:use
   :cl
   :cl-lua.libutil
   :cl-lua.runtime
   :cl-lua.lua-object))
(in-package :cl-lua.baselib)

(define-lua-function "assert" (v &rest args)
  (when (lua-false-p v)
    (let ((message (if args (first args) #L"assertion failed!")))
      (runtime-error message)))
  (apply #'values (cons v args)))

(define-lua-function "collectgarbage" (opt arg))

(defun dolua (stream stream-info)
  (eval
   (cl-lua.translate:translate
    (cl-lua.parser:parse
     (cl-lua.lexer:make-lexer stream stream-info)))))

(define-lua-function "dofile" (filename)
  (if (lua-nil-p filename)
      (dolua *standard-input* "stdin")
      (let ((filename (typecase filename
                        (lua-string
                         (lua-string-to-string filename))
                        (number
                         (prin1-to-string filename))
                        (otherwise
                         (runtime-type-error 1
                                             "string"
                                             (type_ filename))))))
        (with-open-file (stream filename)
          (dolua stream filename)))))

(define-lua-function "error" (message level)
  (runtime-error message))

(define-lua-function "getmetatable" (object)
  (or (get-metamethod object #L"__metatable")
      (let ((metatable (get-metatable object)))
        (if (null metatable)
            +lua-nil+
            metatable))))

(define-lua-function "ipairs" (table)
  (unless (lua-table-p table)
    (runtime-type-error 1 "table" (type_ table)))
  (values #'(lambda (tb i)
              (unless (lua-table-p tb)
                (runtime-type-error 1 "table" (type_ tb)))
              (unless (numberp i)
                (runtime-type-error 2 "number" (type_ i)))
              (incf i)
              (let ((value (lua-table-get tb i nil)))
                (if value
                    (values i value)
                    +lua-nil+)))
          table
          0))

(define-lua-function "print" (&rest args)
  (do ((rest args (cdr rest)))
      ((null rest))
    (princ (tostring (car rest)))
    (if (cdr rest)
        (princ #\tab)
        (terpri)))
  +lua-nil+)

(defun tostring (v)
  (let ((f (get-metamethod v #L"__tostring")))
    (if f
        (funcall f v)
        (lua-object-to-string v))))

(define-lua-function "tostring" (v)
  (tostring v))

(defun type_ (v)
  (cond ((eq +lua-nil+ v)
         "nil")
        ((numberp v)
         "number")
        ((typep v 'lua-string)
         "string")
        ((or (eq +lua-false+ v)
             (eq +lua-true+ v))
         "boolean")
        ((lua-table-p v)
         "table")
        ((functionp v)
         "function")
        ;;thread
        ;;userdata
        ))

(define-lua-function "type" (v)
  (string-to-lua-string (type_ v)))
