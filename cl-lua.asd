(in-package :cl-user)
(defpackage cl-lua-asd
  (:use :cl :asdf))
(in-package :cl-lua-asd)

(defsystem :cl-lua
  :serial t
  :components ((:file "src/util")
               (:file "src/filepos")
               (:file "src/lua-object")
               (:file "src/error")
               (:file "src/token")
               (:file "src/lexer")
               (:file "src/ast")
               (:file "src/convert")
               (:file "src/parser")
               (:file "src/runtime")
               (:file "src/translate")
               (:file "src/lua"))
  :depends-on (:cl-ppcre :alexandria :babel)
  :in-order-to ((test-op (test-op cl-lua-test))))
