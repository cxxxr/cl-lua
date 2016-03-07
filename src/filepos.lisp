(in-package :cl-user)
(defpackage :cl-lua.filepos
  (:use :cl)
  (:export
   :filepos
   :make-filepos
   :filepos-stream
   :filepos-linum))
(in-package :cl-lua.filepos)

(defstruct (filepos (:constructor make-filepos (stream linum)))
  (stream nil :type (or null stream) :read-only t)
  (linum 0 :type integer :read-only t))
