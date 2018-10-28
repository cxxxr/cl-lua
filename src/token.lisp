(defpackage :cl-lua.token
  (:use :cl :cl-lua.filepos)
  (:export
   :*operator-tags*
   :*keyword-tags*
   :*tags*
   :token
   :token-value
   :token-tag
   :token-filepos
   :token-linum
   :make-token
   :tag-equal
   :tag-member
   :eof-token-p))
(in-package :cl-lua.token)

(defparameter *operator-tags*
  (list "+" "-" "*" "/" "%" "^" "#" "&" "~" "|" "<<" ">>" "//"
        "==" "~=" "<=" ">=" "<" ">" "=" "(" ")" "{" "}"
        "[" "]" "::" ";" ":" "," "." ".." "..."))

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

(defstruct (token (:constructor make-token-internal (value tag filepos)))
  (value nil :read-only t)
  (tag "" :type string :read-only t)
  (filepos nil :type filepos :read-only t))

(defmethod print-object ((token token) stream)
  (format stream
          "<~D:~A:~A>"
          (token-linum token)
          (token-tag token)
          (token-value token)))

(defun make-token (value &key tag filepos)
  (assert (tag-member tag *tags*))
  (make-token-internal value tag filepos))

(defun token-linum (token)
  (check-type token token)
  (filepos-linum (token-filepos token)))

(defun tag-equal (tag1 tag2)
  (equal tag1 tag2))

(defun tag-member (tag1 tags)
  (member tag1 tags :test #'tag-equal))

(defun eof-token-p (token)
  (tag-equal (token-tag token) "eof"))
