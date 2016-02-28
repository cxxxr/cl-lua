(in-package :cl-user)
(defpackage :cl-lua-test.parser
  (:use :cl :cl-lua.parser :cl-lua.lexer :cl-lua.ast)
  (:export :test-all))
(in-package :cl-lua-test.parser)

(defun test (string ast &optional (test #'equal))
  (prove:is (parse-from-string string) ast :test test))

(defun test-all ()
  (prove:plan nil)
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
  (test "return 1*2"
        '(:block
          nil
          (:return ((:binary-op "*"
                                (:number 1)
                                (:number 2))))))
  (test "return 1+2*3"
        '(:block
          nil
          (:return ((:binary-op "+"
                                (:number 1)
                                (:binary-op "*"
                                            (:number 2)
                                            (:number 3)))))))
  (test "return -3+4"
        '(:block
          nil
          (:return ((:binary-op "+"
                                (:unary-op "-" (:number 3))
                                (:number 4))))))
  (test "return -2^3"
        '(:block
          nil
          (:return ((:unary-op "-"
                               (:binary-op "^"
                                           (:number 2)
                                           (:number 3)))))))
  (test "return 1*2+3"
        '(:block
          nil
          (:return ((:binary-op "+"
                                (:binary-op "*"
                                            (:number 1)
                                            (:number 2))
                                (:number 3))))))
  (test "return (3+4)*2"
        '(:block
          nil
          (:return ((:binary-op "*"
                                (:binary-op "+"
                                            (:number 3)
                                            (:number 4))
                                (:number 2))))))
  (test "return -123^456"
        '(:block
          nil
          (:return ((:unary-op "-"
                               (:binary-op "^"
                                           (:number 123)
                                           (:number 456)))))))
  (test "a=3"
        '(:block
          ((:assign ((:var "a")) ((:number 3))))
          (:void)))
  (test "print(1)"
        '(:block ((:call-function (:var "print") ((:number 1)))) (:void)))
  (test "f(1,2,3)"
        '(:block ((:call-function (:var "f") ((:number 1) (:number 2) (:number 3))))
          (:void)))
  (test "func('abc')"
        `(:block ((:call-function (:var "func")
                                  ((:string ,(cl-lua.util:string-to-bytes "abc")))))
           (:void))
        #'equalp)
  (test "func'abc'"
        `(:block ((:call-function (:var "func")
                                  ((:string ,(cl-lua.util:string-to-bytes "abc")))))
           (:void))
        #'equalp)
  (test "func{1,2,3}"
        '(:block
          ((:call-function (:var "func")
            ((:tableconstructor ((:number 1) (:number 2) (:number 3)) (:void)))))
          (:void)))
  (test "a={1,2,3}"
        '(:block
          ((:assign ((:var "a"))
            ((:tableconstructor ((:number 1) (:number 2) (:number 3))
              (:void)))))
          (:void)))
  (test "::foo::"
        '(:block
          ((:label "foo"))
          (:void)))
  (test "break"
        '(:block
          ((:break))
          (:void)))
  (test "goto foo"
        '(:block
          ((:goto "foo"))
          (:void)))
  (test "do ::foo:: goto foo end"
        '(:block
          ((:block ((:label "foo") (:goto "foo"))
             (:void)))
          (:void)))
  (prove:finalize))
