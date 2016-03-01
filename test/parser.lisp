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
        '(:block nil (:return nil)))
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
  (test "f()g();;h()"
        '(:block
          ((:call-function (:var "f") ())
           (:call-function (:var "g") ())
           (:call-function (:var "h") ()))
          (:void)))
  (test "a=3"
        '(:block
          ((:assign ((:var "a")) ((:number 3))))
          (:void)))
  (test "print(1)"
        '(:block ((:call-function (:var "print") ((:number 1)))) (:void)))
  (test "f(1,2,3)"
        '(:block ((:call-function
                   (:var "f")
                   ((:number 1) (:number 2) (:number 3))))
          (:void)))
  (test "func('abc')"
        `(:block ((:call-function
                   (:var "func")
                   ((:string ,(cl-lua.util:string-to-bytes "abc")))))
           (:void))
        #'equalp)
  (test "func'abc'"
        `(:block ((:call-function
                   (:var "func")
                   ((:string ,(cl-lua.util:string-to-bytes "abc")))))
           (:void))
        #'equalp)
  (test "func{1,2,3}"
        '(:block
          ((:call-function (:var "func")
            ((:tableconstructor
              ((:number 1) (:number 2) (:number 3))
              (:void)))))
          (:void)))
  (test "a={1,2,3}"
        '(:block
          ((:assign ((:var "a"))
            ((:tableconstructor ((:number 1) (:number 2) (:number 3))
              (:void)))))
          (:void)))
  (test "a = { [f(1)] = g; \"x\", \"y\"; x = 1, f(x), [30] = 23; 45 }"
        `(:block
             ((:assign
               ((:var "a"))
               ((:tableconstructor
                 ((:string ,(cl-lua.util:string-to-bytes "x"))
                  (:string ,(cl-lua.util:string-to-bytes "y"))
                  (:call-function (:var "f") ((:var "x")))
                  (:number 45))
                 (((:call-function (:var "f") ((:number 1)))
                   (:var "g"))
                  ((:var "x")
                   (:number 1))
                  ((:number 30)
                   (:number 23)))))))
           (:void))
        #'equalp)
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
  (test "i = 0
while i < 10 do
  print(i)
end"
        '(:block
          ((:assign ((:var "i")) ((:number 0)))
           (:while (:binary-op "<" (:var "i") (:number 10))
            (:block
                ((:call-function (:var "print") ((:var "i"))))
              (:void))))
          (:void)))
  (test "repeat f() g() until test"
        '(:block
          ((:repeat
            (:block ((:call-function (:var "f") ())
                     (:call-function (:var "g") ()))
              (:void))
            (:var "test")))
          (:void)))
  (test "if a == 1 then print(1) end"
        '(:block
          ((:if (:binary-op "=="
                            (:var "a")
                            (:number 1))
                (:block ((:call-function (:var "print") ((:number 1))))
                  (:void))
                (:void)))
          (:void)))
  (test "if a == 1 then print(1) else print(2) end"
        '(:block
          ((:if (:binary-op "=="
                            (:var "a")
                            (:number 1))
                (:block ((:call-function (:var "print") ((:number 1))))
                  (:void))
                (:block ((:call-function (:var "print") ((:number 2))))
                  (:void))))
          (:void)))
  (test "if a == 1 then print(1) elseif a == 2 then print(2) else print(3) end"
        '(:block
          ((:if (:binary-op "=="
                            (:var "a")
                            (:number 1))
                (:block ((:call-function (:var "print") ((:number 1))))
                  (:void))
                (:if (:binary-op "=="
                                 (:var "a")
                                 (:number 2))
                     (:block ((:call-function (:var "print") ((:number 2))))
                       (:void))
                     (:block ((:call-function (:var "print") ((:number 3))))
                       (:void)))))
          (:void)))
  (test "for x = 1, 10 do print(i) end"
        '(:block
          ((:for "x" (:number 1) (:number 10) (:number 1)
            (:block ((:call-function (:var "print") ((:var "i"))))
              (:void))))
          (:void)))
  (test "for x = 1, 10, 2 do print(i) end"
        '(:block
          ((:for "x" (:number 1) (:number 10) (:number 2)
            (:block ((:call-function (:var "print") ((:var "i"))))
              (:void))))
          (:void)))
  (test "for x = f(), 10 do print(i) end"
        '(:block
          ((:for "x" (:call-function (:var "f") ()) (:number 10) (:number 1)
            (:block ((:call-function (:var "print") ((:var "i"))))
              (:void))))
          (:void)))
  (prove:is-condition (parse-from-string "for x, y = 1, 10 do print(x) end")
                      'parser-error)
  (test "for x in explist do print(x) end"
        '(:block
          ((:generic-for ("x") ((:var "explist"))
            (:block ((:call-function (:var "print") ((:var "x"))))
              (:void))))
          (:void)))
  (test "for x,y,z in explist do end"
        '(:block
          ((:generic-for ("x" "y" "z") ((:var "explist"))
            (:block ()
              (:void))))
          (:void)))
  (test "function f() end"
        '(:block ((:assign
                   ((:var "f"))
                   ((:function () (:block () (:void))))))
          (:void)))
  (test "function f(x,y,z) return x+y+z end"
        '(:block ((:assign
                   ((:var "f"))
                   ((:function ("x" "y" "z")
                     (:block ()
                       (:return ((:binary-op "+"
                                             (:binary-op "+"
                                                         (:var "x")
                                                         (:var "y"))
                                             (:var "z")))))))))
          (:void)))
  (test "function f(x,y,...) end"
        '(:block ((:assign
                   ((:var "f"))
                   ((:function ("x" "y" :rest)
                     (:block () (:void))))))
          (:void)))
  (test "function a.b.c() end"
        `(:block ((:assign
                   ((:refer-table
                     (:refer-table
                      (:var "a")
                      (:string ,(cl-lua.util:string-to-bytes "b")))
                     (:string ,(cl-lua.util:string-to-bytes "c"))))
                   ((:function () (:block () (:void))))))
           (:void))
        #'equalp)
  (test "local x"
        '(:block ((:local ("x") (:void)))
          (:void)))
  (test "local x = 0"
        '(:block ((:local ("x") ((:number 0))))
          (:void)))
  (test "local x, y = f()"
        '(:block ((:local ("x" "y") ((:call-function (:var "f") ()))))
          (:void)))
  (test "local x, y = f(), g()"
        '(:block ((:local
                   ("x" "y")
                   ((:call-function (:var "f") ())
                    (:call-function (:var "g") ()))))
          (:void)))
  (test "local function f() end"
        '(:block ((:local ("f") (:void))
                  (:assign ("f")
                   ((:function () (:block () (:void))))))
          (:void)))
  (test "local function f(x) print(x) return x, 1 end"
        '(:block ((:local ("f") (:void))
                  (:assign ("f")
                   ((:function ("x")
                     (:block ((:call-function (:var "print") ((:var "x"))))
                       (:return ((:var "x") (:number 1))))))))
          (:void)))
  (test "local function f(...) print(...) end"
        '(:block ((:local ("f") (:void))
                  (:assign ("f")
                   ((:function (:rest)
                     (:block ((:call-function (:var "print") ((:rest))))
                       (:void))))))
          (:void)))
  (test "f(nil, false, true, 123, 123.45, 'abc', ..., function (x) return x + x end)"
        `(:block ((:call-function
                   (:var "f")
                   ((:nil) (:false) (:true) (:number 123) (:number 123.45)
                    (:string ,(cl-lua.util:string-to-bytes "abc"))
                    (:rest)
                    (:function ("x")
                               (:block ()
                                 (:return ((:binary-op "+"
                                                       (:var "x")
                                                       (:var "x")))))))))
           (:void))
        #'equalp)
  (test "a.b = c"
        `(:block ((:assign ((:refer-table
                             (:var "a")
                             (:string ,(cl-lua.util:string-to-bytes "b"))))
                           ((:var "c"))))
           (:void))
        #'equalp)
  (test "a[b][c] = d"
        '(:block
          ((:assign ((:refer-table
                      (:refer-table
                       (:var "a")
                       (:var "b"))
                      (:var "c")))
            ((:var "d"))))
          (:void)))
  (test "a:b()"
        '(:block
          ((:call-method (:var "a") "b" ()))
          (:void)))
  (test "a.b.c:d(x)"
        `(:block
             ((:call-method
               (:refer-table
                (:refer-table
                 (:var "a")
                 (:string ,(cl-lua.util:string-to-bytes "b")))
                (:string ,(cl-lua.util:string-to-bytes "c")))
               "d"
               ((:var "x"))))
           (:void))
        #'equalp)
  (test "a.b.c:d[[foo]]"
        `(:block
             ((:call-method
               (:refer-table
                (:refer-table
                 (:var "a")
                 (:string ,(cl-lua.util:string-to-bytes "b")))
                (:string ,(cl-lua.util:string-to-bytes "c")))
               "d"
               ((:string ,(cl-lua.util:string-to-bytes "foo")))))
           (:void))
        #'equalp)
  (prove:finalize))
