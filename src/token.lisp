(in-package :cl-user)
(defpackage :cl-lua.token
  (:use :cl)
  (:export
   :*operator-tags*
   :*keyword-tags*
   :*tags*
   :token
   :token-value
   :token-tag
   :token-linum
   :token-stream
   :make-token
   :tag-equal
   :tag-member
   :eof-token-p))
(in-package :cl-lua.token)

(defparameter *operator-tags*
  (sort (list "+" "-" "*" "/" "%" "^" "#" "&" "~" "|" "<<" ">>" "//"
              "==" "~=" "<=" ">=" "<" ">" "=" "(" ")" "{" "}"
              "[" "]" "::" ";" ":" "," "." ".." "...")
        #'>
        :key #'length))

(defparameter *keyword-tags*
  (list "and" "break" "do" "else" "elseif" "end"
        "false" "for" "function" "goto" "if" "in"
        "local" "nil" "not" "or" "repeat" "return"
        "then" "true" "until" "while"
        "eof"))

(defparameter *tags*
  (append (list "word" "string" "number")
          *operator-tags*
          *keyword-tags*))

(defstruct (token (:constructor make-token-internal))
  (value nil :read-only t)
  (tag "" :type string :read-only t)
  (linum 0 :type integer :read-only t)
  (stream *standard-input* :type stream :read-only t))

(defmethod print-object ((token token) stream)
  (format stream
          "<~A:~A:~A:~A>"
          (token-stream token)
          (token-linum token)
          (token-tag token)
          (token-value token)))

(defun make-token (value &key tag linum (stream nil stream-p))
  (assert (tag-member tag *tags*))
  (check-type linum (integer 1 #.most-positive-fixnum))
  (if stream-p
      (make-token-internal :value value
                           :tag tag
                           :linum linum
                           :stream stream)
      (make-token-internal :value value
                           :tag tag
                           :linum linum)))

(defun tag-equal (tag1 tag2)
  (equal tag1 tag2))

(defun tag-member (tag1 tags)
  (member tag1 tags :test #'tag-equal))

(defun eof-token-p (token)
  (tag-equal (token-tag token) "eof"))
