(in-package :cl-user)
(defpackage :cl-lua-test.lexer
  (:use :cl :cl-lua.lexer :cl-lua.token)
  (:export :test-all))
(in-package :cl-lua-test.lexer)

(defun make-lines (&rest lines)
  (format nil "~{~a~%~}" lines))

(defun test (string &rest tokens)
  (loop :for token1 :in (lex-from-string string)
	:for token2 :in tokens
	:do (prove:is token1 token2
		      :test #'equalp)))

(defun skip-space-and-comment-test ()
  (test (make-lines ""
		    "    "
		    "--coment"
		    "--[==[long comment"
		    "  "
		    "aaaaaaaaaaa"
		    "--]==]"
		    "--[=[   ]=]"
		    "    --[===["
		    "           ---]===]"
		    ""
		    "   +")
	(make-token "+" :tag "+" :linum 12)))

(defun operators-test ()
  (dolist (op '("..." "<<" ">>" "//" "==" "~=" "<=" ">=" "::" ".." "+" "-" "*" "/" "%" "^" "#"
                "&" "~" "|" "<" ">" "=" "(" ")" "{" "}" "[" "]" ";" ":" "," "."))
    (prove:is (if (tag-member op *operator-tags*)
                  t
                  nil)
              t))
  (apply #'test
         (format nil "~{~a ~}" *operator-tags*)
         (mapcar #'(lambda (op)
                     (make-token op :tag op :linum 1))
                 *operator-tags*)))

(defun word-test ()
  (apply #'test
	 (format nil "~{~a ~}" *keyword-tags*)
	 (mapcar #'(lambda (word)
		     (make-token word :tag word :linum 1))
		 *keyword-tags*))
  (let ((names (list "abc" "ABC" "aBc" "Abc" "_xyz" "_10" "_d10" "a10")))
    (apply #'test
	   (apply #'make-lines names)
	   (loop :for name :in names
		 :for linum :from 1
		 :collect (make-token name :tag "word" :linum linum)))))

(defun string-test ()
  (test (format nil "~{~a~^ ~}"
		(list "'abc'"
		      (MAKE-LINES "" "" "'abc'")
		      "'\\a\\b\\f\\n\\r\\t\\v\\\\\\\"\\''"
		      (MAKE-LINES "'foo\\z bar'")
		      (MAKE-LINES "'\\z bar'")
		      (MAKE-LINES "'\\zbar'")
		      "\"\\aa\\a\""
		      "'\\x61\\x62\\x63\\x5F\\x5f'"
		      "'\\x611'"
		      "'1\\x611'"
		      "'\\061\\062\\063'"
		      "'foo\\061\\062\\063bar'"
		      "'\\u{3042}\\u{3043}\\u{3044}'" ))
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS "abc") :TAG "string" :LINUM 1)
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS "abc") :TAG "string" :LINUM 3)
	(MAKE-TOKEN
	 (BABEL:STRING-TO-OCTETS
	  (COERCE
	   (LIST #\Bel #\Backspace #\Page #\Newline #\Return #\Tab #\Vt #\\ #\" #\')
	   'STRING))
	 :TAG "string" :LINUM 4)
	(MAKE-TOKEN #(102 111 111 98 97 114) :TAG "string" :LINUM 4)
	(MAKE-TOKEN #(98 97 114) :TAG "string" :LINUM 5)
	(MAKE-TOKEN #(98 97 114) :TAG "string" :LINUM 6)
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS (COERCE (LIST #\Bel #\a #\Bel) 'STRING))
		    :TAG "string" :LINUM 7)
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS "abc__") :TAG "string" :LINUM 7)
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS "a1") :TAG "string" :LINUM 7)
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS "1a1") :TAG "string" :LINUM 7)
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS "=>?") :TAG "string" :LINUM 7)
	(MAKE-TOKEN (BABEL:STRING-TO-OCTETS "foo=>?bar") :TAG "string" :LINUM 7)
	(MAKE-TOKEN
	 (BABEL:STRING-TO-OCTETS (MAP 'STRING #'CODE-CHAR (VECTOR 12354 12355 12356)))
	 :TAG "string" :LINUM 7)))

(defun long-string-test ()
  (test "[[abcd]]"
	(make-token #(97 98 99 100) :tag "string" :linum 1))
  (test (make-lines ""
		    ""
		    "[["
		    "abc"
		    "xyz]]"
		    ""
		    "[[x]]"
		    "[===["
		    "abcdefg"
		    "foooooooo"
		    "]==]"
		    "]====]"
		    ""
		    "]===]")
	(make-token #(97 98 99 10 120 121 122) :tag "string" :linum 3)
	(make-token #(120) :tag "string" :linum 7)
	(make-token (map 'vector #'char-code (make-lines "abcdefg" "foooooooo" "]==]" "]====]" ""))
		    :tag "string"
		    :linum 8)))

(defun digit-number-test ()
  (test (format nil "~{~a~^ ~}"
		'("123"
		  ".1"
		  ".123"
		  "12."
		  "123.456"
		  "10e2"
		  "2E10"
		  "1.2e3"
		  "1.2e10"
		  "1.12e10"
		  "314.16e-2"
		  "314.16e+2"))
	(make-token 123 :tag "number" :linum 1)
	(make-token 0.1 :tag "number" :linum 1)
	(make-token 0.123 :tag "number" :linum 1)
	(make-token 12.0 :tag "number" :linum 1)
	(make-token 123.456 :tag "number" :linum 1)
	(make-token 10e2 :tag "number" :linum 1)
	(make-token 2e10 :tag "number" :linum 1)
	(make-token 1.2e3 :tag "number" :linum 1)
	(make-token 1.2e10 :tag "number" :linum 1)
	(make-token 1.12e10 :tag "number" :linum 1)
	(make-token 314.16e-2 :tag "number" :linum 1)
	(make-token 314.16e+2 :tag "number" :linum 1)))

(defun hex-number-test ()
  (prove:is-error (lex-from-string "0x") lexer-error)
  (test "0xaf1 0x1a.f1 0x.abc 0x0.1E 0xA23p-4 0X1.921FB54442D18P+1"
	(make-token #xaf1 :tag "number" :linum 1)
	(make-token 26.941406 :tag "number" :linum 1)
	(make-token 0.67089844 :tag "number" :linum 1)
	(make-token 0.1171875 :tag "number" :linum 1)
	(make-token 162.1875 :tag "number" :linum 1)
	(make-token 3.1415925 :tag "number" :linum 1)))

(defun test-all ()
  (prove:plan nil)
  (skip-space-and-comment-test)
  (operators-test)
  (word-test)
  (string-test)
  (long-string-test)
  (digit-number-test)
  (hex-number-test)
  (prove:finalize))

(test-all)
