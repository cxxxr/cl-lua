(in-package :cl-user)
(defpackage :cl-lua
  (:use :cl)
  (:export
   :run-file
   :run-string))
(in-package :cl-lua)

(defun run-stream (stream stream-info)
  (let ((x (cl-lua.translate:translate
            (cl-lua.parser:parse
             (cl-lua.lexer:make-lexer stream stream-info)))))
    (pprint x)
    (fresh-line)
    (eval x)))

(defun run-file (filename)
  (with-open-file (in filename)
    (run-stream in filename)))

(defun run-string (string)
  (with-input-from-string (in string)
    (run-stream in nil)))
