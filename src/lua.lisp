(in-package :cl-user)
(defpackage :cl-lua
  (:use :cl)
  (:export
   :do-file))
(in-package :cl-lua)

(defun do-file (filename)
  (with-open-file (in filename)
    (let ((x (cl-lua.translate:translate
              (cl-lua.parser:parse
               (cl-lua.lexer:make-lexer in filename)))))
      (print x)
      (eval x))))
