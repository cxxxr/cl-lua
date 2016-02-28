(in-package :cl-user)
(defpackage :cl-lua.token
  (:use :cl)
  (:export
   :*binary-operator-names*
   :*unary-operator-names*
   :*operator-names*
   :*keyword-names*
   :*tags*
   :token
   :token-value
   :token-tag
   :token-linum
   :make-token
   :tag-equal
   :tag-member
   :eof-token-p))
(in-package :cl-lua.token)

(defparameter *binary-operator-names*
  (list "+" "-" "*" "/" "//" "^" "%"
        "&" "~" "|" ">>" "<<" ".."
        "<" "<=" ">" ">=" "==" "~="
        "and" "or"))

(defparameter *unary-operator-names*
  (list "-" "not" "#" "~"))

(defparameter *operator-names*
  (sort (list "+" "-" "*" "/" "%" "^" "#" "&" "~" "|" "<<" ">>" "//"
              "==" "~=" "<=" ">=" "<" ">" "=" "(" ")" "{" "}"
              "[" "]" "::" ";" ":" "," "." ".." "...")
        #'>
        :key #'length))

(defparameter *keyword-names*
  (list "and" "break" "do" "else" "elseif" "end"
        "false" "for" "function" "goto" "if" "in"
        "local" "nil" "not" "or" "repeat" "return"
        "then" "true" "until" "while"
        "eof"))

(defparameter *tags*
  (append (list "word" "string" "number")
          *operator-names*
          *keyword-names*))

(defstruct (token (:constructor make-token-internal))
  (value nil :read-only t)
  (tag "" :type string :read-only t)
  (linum 0 :type integer :read-only t))

(defmethod print-object ((token token) stream)
  (format stream
          "<~A:~A:~A>"
          (token-linum token)
          (token-tag token)
          (token-value token)))

(defun make-token (value &key tag linum)
  (assert (tag-member tag *tags*))
  (check-type linum (integer 1 #.most-positive-fixnum))
  (make-token-internal :value value
		       :tag tag
		       :linum linum))

(defun tag-equal (tag1 tag2)
  (equal tag1 tag2))

(defun tag-member (tag1 tags)
  (member tag1 tags :test #'tag-equal))

(defun eof-token-p (token)
  (tag-equal (token-tag token) "eof"))
