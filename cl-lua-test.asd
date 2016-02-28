
(defsystem :cl-lua-test
  :depends-on (:cl-lua :prove :babel)
  :components ((:module "test"
		:serial t
		:components ((:file "lexer")
                             (:file "parser")))))
