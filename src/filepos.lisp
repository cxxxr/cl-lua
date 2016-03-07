(in-package :cl-user)
(defpackage :cl-lua.filepos
  (:use :cl)
  (:export
   :filepos
   :make-filepos
   :filepos-stream-info
   :filepos-linum))
(in-package :cl-lua.filepos)

(defstruct (filepos (:constructor make-filepos (stream-info linum)))
  (stream-info nil :read-only t)
  (linum 0 :type integer :read-only t))

(defmethod print-object ((filepos filepos) stream)
  (format stream
          "<~A:~A>"
          (filepos-stream-info filepos)
          (filepos-linum filepos)))
