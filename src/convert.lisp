(in-package :cl-user)
(defpackage :cl-lua.convert
  (:use :cl :alexandria :cl-lua.ast)
  (:export :convert))
(in-package :cl-lua.convert)

(defun convert (x)
  (destructuring-ecase x
    ((:block stats)
     (make-ast :block 1
               (mapcar #'convert stats)))
    ((:return explist)
     (make-ast :return 1
               (mapcar #'convert explist)))
    ((:label name)
     (make-ast :label 1 name))
    ((:goto name)
     (make-ast :goto 1 name))
    ((:break)
     (make-ast :break 1))
    ((:while exp body)
     (make-ast :while 1 (convert exp) (convert body)))
    ((:repeat body exp)
     (make-ast :repeat 1 (convert body) (convert exp)))
    ((:if test then else)
     (make-ast :if 1 (convert test) (convert then) (convert else)))
    ((:for name init end step body)
     (make-ast :for
               1
               name
               (convert init)
               (convert end)
               (convert step)
               (convert body)))
    ((:generic-for namelist explist body)
     (make-ast :generic-for
               1
               namelist
               (mapcar #'convert explist)
               (convert body)))
    ((:local namelist explist)
     (make-ast :local
               1
               namelist
               (if (ast-void-p explist)
                   (convert explist)
                   (mapcar #'convert explist))))
    ((:assign varlist explist)
     (make-ast :assign
               1
               (mapcar #'convert varlist)
               (mapcar #'convert explist)))
    ((:var name)
     (make-ast :var 1 name))
    ((:nil)
     (make-ast :nil 1))
    ((:false)
     (make-ast :false 1))
    ((:true)
     (make-ast :true 1))
    ((:number value)
     (make-ast :number 1 value))
    ((:string value)
     (make-ast :string 1 value))
    ((:tableconstructor field-sequence field-pairs)
     (make-ast :tableconstructor 1
               (if (ast-void-p field-sequence)
                   (convert field-sequence)
                   (mapcar #'convert field-sequence))
               (if (ast-void-p field-pairs)
                   (convert field-pairs)
                   (mapcar #'(lambda (elt)
                               (list (convert (car elt))
                                     (convert (cadr elt))))
                           field-pairs))))
    ((:rest)
     (make-ast :rest 1))
    ((:unary-op name exp)
     (make-ast :unary-op 1 name (convert exp)))
    ((:binary-op name left right)
     (make-ast :binary-op 1 name (convert left) (convert right)))
    ((:function parameters body)
     (make-ast :function 1
               parameters
               (convert body)))
    ((:refer-table key value)
     (make-ast :refer-table 1
               (convert key)
               (convert value)))
    ((:call-function fun args)
     (make-ast :call-function 1 (convert fun) (mapcar #'convert args)))
    ((:call-method prefix name args)
     (make-ast :call-method 1
               (convert prefix)
               name
               (mapcar #'convert args)))
    ((:void)
     (make-ast :void nil))))
