(defsystem "cl-lua"
  :serial t
  :components ((:module "src/"
                :components ((:file "util")
                             (:file "filepos")
                             (:file "lua-object")
                             (:file "error")
                             (:file "token")
                             (:file "lexer")
                             (:file "ast")
                             (:file "convert")
                             (:file "parser")
                             (:file "runtime")
                             (:file "translate")
                             (:file "libutil")
                             (:file "baselib")
                             (:file "lua"))))
  :depends-on ("cl-ppcre" "alexandria" "babel")
  :in-order-to ((test-op (test-op cl-lua-test))))
