(in-package :cl-user)
(defpackage :cl-lua-test.parser
  (:use :cl :cl-lua.parser :cl-lua.lexer :cl-lua.ast)
  (:export :test-all))
(in-package :cl-lua-test.parser)

(defun test (string ast)
  (prove:is (parse-from-string string) ast :test #'equal))

(defun test-all ()
  (test "return"
        '(:block nil (:return (:void))))
  (test "return 1"
        '(:block
          nil
          (:return ((:number 1)))))
  (test "return 1+2"
        '(:block
          nil
          (:return ((:binary-op "+" (:number 1) (:number 2))))))
  (test "return 1+2+3"
        '(:block
          nil
          (:return ((:binary-op "+"
                                (:binary-op "+"
                                            (:number 1)
                                            (:number 2))
                                (:number 3))))))
  )
