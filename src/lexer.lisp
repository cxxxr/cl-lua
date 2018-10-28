(defpackage :cl-lua.lexer
  (:use
   :cl
   :cl-lua.filepos
   :cl-lua.token
   :cl-lua.lua-object
   :cl-lua.util
   :cl-lua.error)
  (:import-from
   :alexandria
   :with-gensyms
   :once-only)
  (:export
   :make-lexer
   :with-lexer-from-string
   :lex
   :lex-from-string))
(in-package :cl-lua.lexer)

(defstruct (lexer (:constructor make-lexer-internal))
  stream
  stream-info
  line
  linum
  column
  eof-p)

(defun make-lexer (stream stream-info)
  (make-lexer-internal :stream stream
                       :stream-info stream-info
		       :line ""
		       :linum 0
		       :column 0
                       :eof-p nil))

(defmacro with-lexer-from-string ((lexer string) &body body)
  (check-type lexer symbol)
  (with-gensyms (gstream)
    `(with-input-from-string (,gstream ,string)
       (let ((,lexer (make-lexer ,gstream nil)))
         ,@body))))

(defun raise-lexer-error (lexer condition
                          &optional (column (lexer-column lexer)))
  (error condition
         :filepos (make-filepos (lexer-stream-info lexer)
                                (lexer-linum lexer))
	 :near (subseq (lexer-line lexer) column)))

(defmacro lexer-scan-regex (lexer regex)
  (once-only (lexer)
    `(ppcre:scan ,regex
                 (lexer-line ,lexer)
                 :start (lexer-column ,lexer))))

(defun lexer-scan (lexer string)
  (string= (lexer-line lexer)
           string
           :start1 (lexer-column lexer)
           :end1 (min (+ (lexer-column lexer)
                         (length string))
                      (length (lexer-line lexer)))))

(defun next-line (lexer)
  (unless (lexer-eof-p lexer)
    (let ((line (read-line (lexer-stream lexer) nil)))
      (incf (lexer-linum lexer))
      (setf (lexer-column lexer) 0)
      (cond ((null line)
             (setf (lexer-eof-p lexer) t)
             (setf (lexer-line lexer) "")
             nil)
            (t
             (setf (lexer-line lexer) line)
             t)))))

(defun end-column-p (lexer)
  (>= (lexer-column lexer)
      (length (lexer-line lexer))))

(defun ahead-char (lexer)
  (schar (lexer-line lexer) (lexer-column lexer)))

(defun read-line-while-empty (lexer)
  (loop :while (end-column-p lexer)
	:do (unless (next-line lexer)
              (return))))

(defun space-char-p (c)
  (member c '(#\space #\tab)))

(defun skip-space (lexer)
  (loop :while (not (end-column-p lexer))
	:for c := (ahead-char lexer)
	:if (space-char-p c)
	  :do (incf (lexer-column lexer))
	:else
	  :do (return nil)
	:finally (return t)))

(defun skip-space-lines (lexer)
  (loop
    (read-line-while-empty lexer)
    (when (lexer-eof-p lexer)
      (return))
    (unless (skip-space lexer)
      (return))))

(defun skip-comment (lexer)
  (when (lexer-scan lexer "--")
    (incf (lexer-column lexer) 2)
    (multiple-value-bind (start end)
	(lexer-scan-regex lexer "^\\[=*\\[")
      (cond (start
	     (setf (lexer-column lexer) end)
	     (scan-long-string lexer
                               (- end start 2)
                               nil
                               'unfinished-long-comment-error))
	    (t
	     (next-line lexer))))
    t))

(defun skip-space-and-comment (lexer)
  (loop
    (skip-space-lines lexer)
    (unless (skip-comment lexer)
      (return))))

(defun scan-long-string (lexer n fn eof-error)
  (loop :with string := (concatenate 'string
                                     "]"
                                     (make-string n :initial-element #\=)
                                     "]")
        :and start-linum := (lexer-linum lexer)
        :do (let ((pos (search string
                               (lexer-line lexer)
                               :start2 (lexer-column lexer))))
              (when fn
                (funcall fn
                         (if (and (null pos)
                                  (zerop (lexer-column lexer)))
                             (lexer-line lexer)
                             (subseq (lexer-line lexer)
                                     (lexer-column lexer)
                                     pos))
                         (null pos)))
              (when pos
                (setf (lexer-column lexer) (+ pos 2 n))
                (return)))
            (next-line lexer)
            (when (lexer-eof-p lexer)
              (error eof-error
                     :filepos (make-filepos (lexer-stream-info lexer)
                                            (lexer-linum lexer))
                     :start-linum start-linum
                     :near "<eof>"))))

(let ((operators
        (sort (copy-list *operator-tags*)
              #'>
              :key #'length)))
  (defun try-scan-operator (lexer)
    (dolist (op operators)
      (when (lexer-scan lexer op)
        (incf (lexer-column lexer)
              (length op))
        (return
          (make-token op
                      :tag op
                      :filepos (make-filepos (lexer-stream-info lexer)
                                             (lexer-linum lexer))))))))

(defun try-scan-word (lexer)
  (multiple-value-bind (s e)
      (lexer-scan-regex lexer "^[a-zA-Z_][a-zA-Z0-9_]*")
    (when s
      (setf (lexer-column lexer) e)
      (let ((str (subseq (lexer-line lexer) s e)))
	(make-token str
		    :tag (if (tag-member str *keyword-tags*)
                             str
                             "word")
                    :filepos (make-filepos (lexer-stream-info lexer)
                                           (lexer-linum lexer)))))))

(defun try-scan-string (lexer)
  (let ((quote-char (ahead-char lexer))
        (start-column (lexer-column lexer)))
    (labels ((ahead-char-with-eof-handle
                 (lexer)
               (if (end-column-p lexer)
                   (raise-lexer-error lexer
                                      'unfinished-string-error
                                      start-column)
                   (ahead-char lexer))))
      (when (or (char= quote-char #\")
                (char= quote-char #\'))
        (incf (lexer-column lexer))
        (loop :with chars := nil
              :and start-linum := (lexer-linum lexer)
              :for c := (ahead-char-with-eof-handle lexer)
              :do (cond
                    ((char= c quote-char)
                     (incf (lexer-column lexer))
                     (return-from try-scan-string
                       (make-token (coerce (nreverse chars) 'lua-string)
                                   :tag "string"
                                   :filepos (make-filepos
                                             (lexer-stream-info lexer)
                                             start-linum))))
                    ((char= c #\\)
                     (incf (lexer-column lexer))
                     (let* ((esc-char (if (end-column-p lexer)
                                          (progn
                                            (setq start-column 0)
                                            (next-line lexer)
                                            #\newline)
                                          (prog1 (ahead-char lexer)
                                            (incf (lexer-column lexer)))))
                            (sp-char (case esc-char
                                       (#\a #\Bel)
                                       (#\b #\Backspace)
                                       (#\f #\Page)
                                       (#\n #\Newline)
                                       (#\r #\Return)
                                       (#\t #\Tab)
                                       (#\v #\Vt)
                                       ((#\\ #\' #\" #\newline)
                                        esc-char))))
                       (cond
                         (sp-char
                          (push (char-code sp-char) chars))
                         ((char= esc-char #\z)
                          (cond ((end-column-p lexer)
                                 (next-line lexer))
                                ((space-char-p (ahead-char lexer))
                                 (incf (lexer-column lexer)))))
                         ((char= esc-char #\x)
                          (let ((hexstr (make-string 2)))
                            (dotimes (i 2)
                              (let ((c (char-upcase
                                        (ahead-char-with-eof-handle lexer))))
                                (cond ((or (char<= #\0 c #\9)
                                           (char<= #\A c #\F))
                                       (setf (aref hexstr i) c)
                                       (incf (lexer-column lexer)))
                                      (t
                                       (raise-lexer-error lexer
                                                          'string-hex-error
                                                          start-column)))))
                            (push (parse-integer hexstr :radix 16)
                                  chars)))
                         ((char<= #\0 esc-char #\9)
                          (let ((digit-str (make-string 3)))
                            (setf (aref digit-str 0) esc-char)
                            (dotimes (i 2)
                              (let ((c (ahead-char-with-eof-handle lexer)))
                                (cond ((char<= #\0 c #\9)
                                       (setf (aref digit-str (1+ i)) c)
                                       (incf (lexer-column lexer)))
                                      (t
                                       (return)))))
                            (push (parse-integer digit-str :junk-allowed t)
                                  chars)))
                         ((char= esc-char #\u)
                          (multiple-value-bind (start end)
                              (lexer-scan-regex lexer "^{[a-zA-Z0-F]+}")
                            (unless start
                              (raise-lexer-error lexer
                                                 'escape-sequence-error
                                                 start-column))
                            (dolist (code (unicode-to-utf8
                                           (parse-integer
                                            (subseq (lexer-line lexer)
                                                    (1+ start)
                                                    (1- end))
                                            :radix 16)))
                              (push code chars))
                            (setf (lexer-column lexer) end)))
                         (t
                          (raise-lexer-error lexer
                                             'escape-sequence-error
                                             start-column)))))
                    (t
                     (incf (lexer-column lexer))
                     (let ((code (char-code c)))
                       (if (<= 0 code 255)
                           (push code chars)
                           (dolist (code (unicode-to-utf8 code))
                             (push code chars)))))))))))

(defun try-scan-long-string (lexer)
  (multiple-value-bind (s e)
      (lexer-scan-regex lexer "^\\[=*\\[")
    (when s
      (let ((start-linum (lexer-linum lexer))
            (lua-string (make-empty-lua-string)))
	(if (= e (length (lexer-line lexer)))
            (next-line lexer)
            (setf (lexer-column lexer) e))
	(scan-long-string lexer
			  (- e s 2)
			  (lambda (str newline-p)
                            (setf lua-string
                                  (concatenate 'lua-string
                                               lua-string
                                               (string-to-lua-string str)))
			    (when newline-p
                              (setf lua-string
                                    (concatenate 'lua-string
                                                 lua-string
                                                 (string-to-lua-string
                                                  (string #\newline))))))
                          'unfinished-long-string-error)
	(make-token lua-string
		    :tag "string"
                    :filepos (make-filepos (lexer-stream-info lexer)
                                           start-linum))))))

(defun try-scan-number (lexer)
  (when (lexer-scan-regex lexer "^(?:\\.[0-9]|[0-9])")
    (multiple-value-bind (value end)
        (lua-parse-number (lexer-line lexer)
                          :start (lexer-column lexer)
                          :junk-allowed t)
      (unless value
        (raise-lexer-error lexer 'malformed-number-error))
      (setf (lexer-column lexer) end)
      (make-token value
                  :tag "number"
                  :filepos (make-filepos (lexer-stream-info lexer)
                                         (lexer-linum lexer))))))

(defun make-eof-token (lexer)
  (make-token "EOF"
              :tag "eof"
              :filepos (make-filepos (lexer-stream-info lexer)
                                     (lexer-linum lexer))))

(defun lex (lexer)
  (loop
    (skip-space-and-comment lexer)
    (when (lexer-eof-p lexer)
      (return (make-eof-token lexer)))
    (let ((token (or (try-scan-word lexer)
		     (try-scan-string lexer)
		     (try-scan-long-string lexer)
                     (try-scan-number lexer)
		     (try-scan-operator lexer))))
      (when token
	(return-from lex token))
      (incf (lexer-column lexer)))))

(defun lex-from-string (string)
  (with-input-from-string (stream string)
    (loop :with lexer := (make-lexer stream nil)
	  :for token := (lex lexer)
	  :while (not (eof-token-p token))
	  :collect token)))
