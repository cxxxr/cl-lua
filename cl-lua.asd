(in-package :cl-user)
(defpackage cl-lua-asd
  (:use :cl :asdf))
(in-package :cl-lua-asd)

(defsystem :cl-lua
  :components ((:module "src"
                :serial t
                :components ((:file "util")
                             (:file "token")
                             (:file "error")
                             (:file "lexer")
                             (:file "ast")
                             (:file "parser"))))
  :depends-on (:cl-ppcre :alexandria)
  :in-order-to ((test-op (test-op cl-lua-test))))
