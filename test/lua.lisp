(in-package :cl-user)
(defpackage :cl-lua-test
  (:use :cl))
(in-package :cl-lua-test)

(prove:plan 2)
(prove:subtest "lexer"
  (cl-lua-test.lexer:test))
(prove:subtest "parser"
  (cl-lua-test.parser:test))
(prove:finalize)
