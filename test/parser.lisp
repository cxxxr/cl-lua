(in-package :cl-user)
(defpackage :cl-lua-test.parser
  (:use :cl :cl-lua.parser :cl-lua.lexer :cl-lua.ast)
  (:export :test))
(in-package :cl-lua-test.parser)

(defun is (string ast &optional (test #'equal))
  (prove:is (parse-from-string string)
            (cl-lua.convert:convert ast)
            :test test))

(defun test ()
  (prove:plan 55)
  (is "return"
      '(:block ((:return nil))))
  (is "return 1"
      '(:block
        ((:return ((:number 1))))))
  (is "return 1+2"
      '(:block
        ((:return ((:binary-op "+" (:number 1) (:number 2)))))))
  (is "return 1+2+3"
      '(:block
        ((:return ((:binary-op "+"
                               (:binary-op "+"
                                           (:number 1)
                                           (:number 2))
                               (:number 3)))))))
  (is "return 1*2"
      '(:block
        ((:return ((:binary-op "*"
                               (:number 1)
                               (:number 2)))))))
  (is "return 1+2*3"
      '(:block
        ((:return ((:binary-op "+"
                               (:number 1)
                               (:binary-op "*"
                                           (:number 2)
                                           (:number 3))))))))
  (is "return -3+4"
      '(:block
        ((:return ((:binary-op "+"
                               (:unary-op "-" (:number 3))
                               (:number 4)))))))
  (is "return -2^3"
      '(:block
        ((:return ((:unary-op "-"
                              (:binary-op "^"
                                          (:number 2)
                                          (:number 3))))))))
  (is "return 1*2+3"
      '(:block
        ((:return ((:binary-op "+"
                               (:binary-op "*"
                                           (:number 1)
                                           (:number 2))
                               (:number 3)))))))
  (is "return (3+4)*2"
      '(:block
        ((:return ((:binary-op "*"
                               (:binary-op "+"
                                           (:number 3)
                                           (:number 4))
                               (:number 2)))))))
  (is "return -123^456"
      '(:block
        ((:return ((:unary-op "-"
                              (:binary-op "^"
                                          (:number 123)
                                          (:number 456))))))))
  (is "f()g();;h()"
      '(:block
        ((:call-function (:var "f") ())
         (:call-function (:var "g") ())
         (:call-function (:var "h") ()))))
  (is "a=3"
      '(:block
        ((:assign ((:var "a")) ((:number 3))))))
  (is "a,b=1,2"
      '(:block
        ((:assign ((:var "a") (:var "b")) ((:number 1) (:number 2))))))
  (is "a,b=f()"
      '(:block
        ((:assign ((:var "a") (:var "b")) ((:call-function (:var "f") ()))))))
  (is "print(1)"
      '(:block ((:call-function (:var "print") ((:number 1))))))
  (is "f(1,2,3)"
      '(:block ((:call-function
                 (:var "f")
                 ((:number 1) (:number 2) (:number 3))))))
  (is "func('abc')"
      `(:block ((:call-function
                 (:var "func")
                 ((:string ,(cl-lua.runtime:string-to-lua-string "abc"))))))
      #'equalp)
  (is "func'abc'"
      `(:block ((:call-function
                 (:var "func")
                 ((:string ,(cl-lua.runtime:string-to-lua-string "abc"))))))
      #'equalp)
  (is "func{1,2,3}"
      '(:block
        ((:call-function (:var "func")
          ((:tableconstructor
            ((:number 1) (:number 2) (:number 3))
            (:void)))))))
  (is "a={1,2,3}"
      '(:block
        ((:assign ((:var "a"))
          ((:tableconstructor ((:number 1) (:number 2) (:number 3))
            (:void)))))))
  (is "a = { [f(1)] = g; \"x\", \"y\"; x = 1, f(x), [30] = 23; 45 }"
      `(:block
           ((:assign
             ((:var "a"))
             ((:tableconstructor
               ((:string ,(cl-lua.runtime:string-to-lua-string "x"))
                (:string ,(cl-lua.runtime:string-to-lua-string "y"))
                (:call-function (:var "f") ((:var "x")))
                (:number 45))
               (((:call-function (:var "f") ((:number 1)))
                 (:var "g"))
                ((:var "x")
                 (:number 1))
                ((:number 30)
                 (:number 23))))))))
      #'equalp)
  (is "::foo::"
      '(:block
        ((:label "foo"))))
  (is "break"
      '(:block
        ((:break))))
  (is "goto foo"
      '(:block
        ((:goto "foo"))))
  (is "do ::foo:: goto foo end"
      '(:block
        ((:block ((:label "foo") (:goto "foo"))))))
  (is "i = 0 while i < 10 do print(i) end"
      '(:block
        ((:assign ((:var "i")) ((:number 0)))
         (:while (:binary-op "<" (:var "i") (:number 10))
          (:block ((:call-function (:var "print") ((:var "i")))))))))
  (is "repeat f() g() until test"
      '(:block
        ((:repeat
          (:block ((:call-function (:var "f") ())
                   (:call-function (:var "g") ())))
          (:var "test")))))
  (is "if a == 1 then print(1) end"
      '(:block
        ((:if (:binary-op "=="
                          (:var "a")
                          (:number 1))
              (:block ((:call-function (:var "print") ((:number 1)))))
              (:void)))))
  (is "if a == 1 then print(1) else print(2) end"
      '(:block
        ((:if (:binary-op "=="
                          (:var "a")
                          (:number 1))
              (:block ((:call-function (:var "print") ((:number 1)))))
              (:block ((:call-function (:var "print") ((:number 2)))))))))
  (is "if a == 1 then print(1) elseif a == 2 then print(2) else print(3) end"
      '(:block
        ((:if (:binary-op "=="
                          (:var "a")
                          (:number 1))
              (:block ((:call-function (:var "print") ((:number 1)))))
              (:if (:binary-op "=="
                               (:var "a")
                               (:number 2))
                   (:block ((:call-function (:var "print")
                                            ((:number 2)))))
                   (:block ((:call-function (:var "print")
                                            ((:number 3))))))))))
  (is "for x = 1, 10 do print(i) end"
      '(:block
        ((:for "x" (:number 1) (:number 10) (:number 1)
          (:block ((:call-function (:var "print") ((:var "i")))))))))
  (is "for x = 1, 10, 2 do print(i) end"
      '(:block
        ((:for "x" (:number 1) (:number 10) (:number 2)
          (:block ((:call-function (:var "print") ((:var "i")))))))))
  (is "for x = f(), 10 do print(i) end"
      '(:block
        ((:for "x" (:call-function (:var "f") ()) (:number 10) (:number 1)
          (:block ((:call-function (:var "print") ((:var "i")))))))))
  (prove:is-condition (parse-from-string "for x, y = 1, 10 do print(x) end")
                      'parser-error)
  (is "for x in explist do print(x) end"
      '(:block
        ((:generic-for ("x") ((:var "explist"))
          (:block ((:call-function (:var "print") ((:var "x")))))))))
  (is "for x,y,z in explist do end"
      '(:block
        ((:generic-for ("x" "y" "z") ((:var "explist"))
          (:block ())))))
  (is "function f() end"
      '(:block ((:assign
                 ((:var "f"))
                 ((:function () (:block ())))))))
  (is "function f(x,y,z) return x+y+z end"
      '(:block ((:assign
                 ((:var "f"))
                 ((:function ("x" "y" "z")
                   (:block ((:return ((:binary-op "+"
                                                  (:binary-op "+"
                                                              (:var "x")
                                                              (:var "y"))
                                                  (:var "z"))))))))))))
  (is "function f(x,y,...) end"
      '(:block ((:assign
                 ((:var "f"))
                 ((:function ("x" "y" :rest)
                   (:block ())))))))
  (is "function a.b.c() end"
      `(:block ((:assign
                 ((:refer-table
                   (:refer-table
                    (:var "a")
                    (:string ,(cl-lua.runtime:string-to-lua-string "b")))
                   (:string ,(cl-lua.runtime:string-to-lua-string "c"))))
                 ((:function () (:block ()))))))
      #'equalp)
  (is "function a:b() end"
      `(:block ((:assign
                 ((:refer-table
                   (:var "a")
                   (:string ,(cl-lua.runtime:string-to-lua-string "b"))))
                 ((:function ("self") (:block ()))))))
      #'equalp)
  (is "local x"
      '(:block ((:local ("x") (:void)))))
  (is "local x = 0"
      '(:block ((:local ("x") ((:number 0))))))
  (is "local x, y = f()"
      '(:block ((:local ("x" "y") ((:call-function (:var "f") ()))))))
  (is "local x, y = f(), g()"
      '(:block ((:local
                 ("x" "y")
                 ((:call-function (:var "f") ())
                  (:call-function (:var "g") ()))))))
  (is "local function f() end"
      '(:block ((:local ("f") (:void))
                (:assign ((:var "f"))
                 ((:function () (:block ())))))))
  (is "local function f(x) print(x) return x, 1 end"
      '(:block ((:local ("f") (:void))
                (:assign ((:var "f"))
                 ((:function ("x")
                   (:block ((:call-function (:var "print") ((:var "x")))
                            (:return ((:var "x") (:number 1)))))))))))
  (is "local function f(...) print(...) end"
      '(:block ((:local ("f") (:void))
                (:assign ((:var "f"))
                 ((:function (:rest)
                   (:block ((:call-function (:var "print") ((:rest)))))))))))
  (is "f(nil, false, true, 123, 123.45, 'abc', ..., function (x) return x + x end)"
      `(:block ((:call-function
                 (:var "f")
                 ((:nil) (:false) (:true) (:number 123) (:number 123.45)
                  (:string ,(cl-lua.runtime:string-to-lua-string "abc"))
                  (:rest)
                  (:function ("x")
                             (:block
                                 ((:return ((:binary-op "+"
                                                        (:var "x")
                                                        (:var "x")))))))))))
      #'equalp)
  (is "a.b = c"
      `(:block ((:assign ((:refer-table
                           (:var "a")
                           (:string ,(cl-lua.runtime:string-to-lua-string "b"))))
                         ((:var "c")))))
      #'equalp)
  (is "a[b][c] = d"
      '(:block
        ((:assign ((:refer-table
                    (:refer-table
                     (:var "a")
                     (:var "b"))
                    (:var "c")))
          ((:var "d"))))))
  (is "a:b()"
      '(:block
        ((:call-method (:var "a") "b" ()))))
  (is "a.b.c:d(x)"
      `(:block
           ((:call-method
             (:refer-table
              (:refer-table
               (:var "a")
               (:string ,(cl-lua.runtime:string-to-lua-string "b")))
              (:string ,(cl-lua.runtime:string-to-lua-string "c")))
             "d"
             ((:var "x")))))
      #'equalp)
  (is "a.b.c:d[[foo]]"
      `(:block
           ((:call-method
             (:refer-table
              (:refer-table
               (:var "a")
               (:string ,(cl-lua.runtime:string-to-lua-string "b")))
              (:string ,(cl-lua.runtime:string-to-lua-string "c")))
             "d"
             ((:string ,(cl-lua.runtime:string-to-lua-string "foo"))))))
      #'equalp)
  (prove:finalize))
