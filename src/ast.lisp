(in-package :cl-user)
(defpackage :cl-lua.ast
  (:use :cl)
  (:export
   :make-ast
   :<ast>
   :<void>
   :<block>
   :<return>
   :<label>
   :<break>
   :<goto>
   :<while>
   :<repeat>
   :<if>
   :<for>
   :<generic-for>
   :<assign>
   :<function>
   :<local>
   :<nil>
   :<false>
   :<ture>
   :<number>
   :<string>
   :<tableconstructor>
   :<field>
   :<var>
   :<variadic>
   :<refer-table>
   :<call-function>
   :<call-method>
   ))
(in-package :cl-lua.ast)

(defun make-ast (name &rest args)
  (cons name args))

(defclass <ast> ()
  ((linum
    :initarg :linum
    :reader ast-linum
    :type integer)))

(defclass <void> (<ast>)
  ())

(defclass <block> (<ast>)
  ((stats
    :initarg :stats
    :reader block-stats
    :type list)
   (retstat
    :initarg :retstat
    :reader block-retstat
    :type (or <return> <void>))))

(defclass <return> (<ast>)
  ((explist
    :initarg :explist
    :reader return-explist
    :type (or <void> cons))))

(defclass <label> (<ast>)
  ((name
    :initarg :name
    :reader label-name
    :type string)))

(defclass <break> (<ast>)
  ())

(defclass <goto> (<ast>)
  ((name
    :initarg :name
    :reader goto-name
    :type string)))

(defclass <while> (<ast>)
  ((exp
    :initarg :exp
    :reader while-exp
    :type <exp>)
   (body
    :initarg :body
    :reader while-body
    :type <block>)))

(defclass <repeat> (<ast>)
  ((body
    :initarg :body
    :reader repeat-body
    :type <block>)
   (exp
    :initarg :exp
    :reader repeat-exp
    :type <exp>)))

(defclass <if> (<ast>)
  ((test
    :initarg :test
    :reader if-test
    :type <exp>)
   (then
    :initarg :then
    :reader if-then
    :type <block>)
   (else
    :initarg :else
    :reader if-else
    :type <block>)))

(defclass <for> (<ast>)
  ((name
    :initarg :name
    :reader for-name
    :type <var>)
   (init
    :initarg :init
    :reader for-init
    :type <exp>)
   (end
    :initarg :end
    :reader for-end
    :type <exp>)
   (step
    :initarg :step
    :reader for-step
    :type <exp>)
   (body
    :initarg :body
    :reader for-body
    :type <block>)))

(defclass <generic-for> (<ast>)
  ((namelist
    :initarg :namelist
    :reader generic-for-namelist
    :type cons)
   (explist
    :initarg :explist
    :reader generic-for-explist
    :type cons)
   (body
    :initarg :body
    :reader generic-for-body
    :type <block>)))

(defclass <assign> (<ast>)
  ((varlist
    :initarg :varlist
    :reader assign-varlist
    :type cons)
   (explist
    :initarg :explist
    :reader assign-explist
    :type cons)))

(defclass <function> (<ast>)
  ((parameters
    :initarg :parameters
    :reader function-parameters
    :type list)
   (body
    :initarg :body
    :reader function-body
    :type <block>)))

(defclass <local> (<ast>)
  ((namelist
    :initarg :namelist
    :reader local-namelist
    :type cons)
   (explist
    :initarg :explist
    :reader local-explist
    :type (or <void> cons))))

(defclass <nil> (<ast>)
  ())

(defclass <false> (<ast>)
  ())

(defclass <ture> (<ast>)
  ())

(defclass <number> (<ast>)
  ((value
    :initarg :value
    :reader number-value
    :type number)))

(defclass <string> (<ast>)
  ((chars
    :initarg :chars
    :reader string-chars
    :type (vector (unsigned-byte 8)))))

(defclass <tableconstructor> (<ast>)
  ((array
    :initarg :array
    :reader tableconstructor-array
    :type (or <void> list))
   (pairs
    :initarg :pairs
    :reader tableconstructor-pairs
    :type (or <void> list))))

(defclass <field> (<ast>)
  ((key
    :initarg :key
    :reader field-key
    :type <ast>)
   (value
    :initarg :value
    :reader field-value
    :type <ast>)))

(defclass <var> (<ast>)
  ((name
    :initarg :name
    :reader var-name
    :type string)))

(defclass <variadic> (<var>)
  ((name
    :reader var-name
    :initform "..."
    :type string)))

(defclass <refer-table> (<ast>)
  ((prefix
    :initarg :prefix
    :reader :refer-table-prefix
    :type <ast>)
   (key
    :initarg :key
    :reader :refer-table-key
    :type <ast>)))

(defclass <call-function> (<ast>)
  ((prefix
    :initarg :prefix
    :reader call-function-prefix
    :type <ast>)
   (args
    :initarg :args
    :reader call-function-args
    :type list)))

(defclass <call-method> (<ast>)
  ((prefix
    :initarg :prefix
    :reader call-method-prefix
    :type <ast>)
   (args
    :initarg :args
    :reader call-method-args
    :type list)
   (name
    :initarg :name
    :reader call-method-name
    :type <var>)))
