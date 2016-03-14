(in-package :cl-user)
(defpackage :cl-lua.libutil
  (:use :cl :cl-lua.lua-object)
  (:export :define-lua-function))
(in-package :cl-lua.libutil)

(defmacro define-lua-function (name lambda-list &body body)
  (check-type name string)
  `(cl-lua.runtime:set-global-variable
    ,(string-to-lua-string name)
    #'(lambda ,lambda-list ,@body)))
