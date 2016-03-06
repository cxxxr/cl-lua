(in-package :cl-user)
(defpackage cl-lua-asd
  (:use :cl :asdf))
(in-package :cl-lua-asd)

(defsystem :cl-lua
  :components ((:module "src"
                :serial t
                :components ((:file "util")
                             (:file "lua-object")
                             (:file "token")
                             (:file "error")
                             (:file "lexer")
                             (:file "ast")
                             (:file "convert")
                             (:file "parser")
                             (:file "runtime")
                             (:file "translate"))))
  :depends-on (:cl-ppcre :alexandria :babel)
  :in-order-to ((test-op (test-op cl-lua-test))))
