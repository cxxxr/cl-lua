(defpackage :cl-lua-test
  (:use :cl))
(in-package :cl-lua-test)

(prove:plan 3)
(prove:subtest "lexer"
  (cl-lua-test.lexer:test))
(prove:subtest "parser"
  (cl-lua-test.parser:test))
(prove:subtest "execute"
  (cl-lua-test.execute:test))
(prove:finalize)
