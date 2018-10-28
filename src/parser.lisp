(defpackage :cl-lua.parser
  (:use
   :cl
   :cl-lua.util
   :cl-lua.lexer
   :cl-lua.error
   :cl-lua.token
   :cl-lua.ast
   :cl-lua.lua-object)
  (:import-from
   :alexandria
   :destructuring-ecase)
  (:export
   :parse
   :parse-from-string))
(in-package :cl-lua.parser)

(defvar *lexer*)
(defvar *lookahead*)
(defvar *lookahead-undo-stack*)

(defun syntax-error (token)
  (error 'syntax-error
         :filepos (token-filepos token)
         :value (token-value token)))

(defun pushback (token)
  (push *lookahead* *lookahead-undo-stack*)
  (setf *lookahead* token))

(defun next ()
  (prog1 *lookahead*
    (setf *lookahead*
          (or (pop *lookahead-undo-stack*)
              (lex *lexer*)))))

(defun match-p (tag)
  (tag-equal (token-tag *lookahead*) tag))

(defun match-or (&rest tags)
  (some #'match-p tags))

(defun eof-p ()
  (eof-token-p *lookahead*))

(defun accept (tag)
  (when (match-p tag)
    (next)))

(defun exact (expected-tag)
  (let ((tag (token-tag *lookahead*)))
    (unless (tag-equal tag expected-tag)
      (syntax-error *lookahead*))
    (next)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun case-token-gen-clauses (clauses)
    (mapcar #'(lambda (clause)
                (let ((test (car clause))
                      (body (cdr clause)))
                  `(,(etypecase test
                       (cons
                        `(or ,@(mapcar #'(lambda (tag)
                                           `(match-p ,tag))
                                       test)))
                       (string
                        `(match-p ,test)))
                    ,@body)))
            clauses))

  (defun case-token-collect-tags (clauses)
    (mapcan #'(lambda (c)
                (etypecase (car c)
                  (cons (copy-list (car c)))
                  (string (list (car c)))))
            clauses)))

(defmacro case-token (&body clauses)
  `(cond ,@(case-token-gen-clauses
            (remove-if #'(lambda (cl)
                           (and (symbolp (car cl))
                                (string-equal "otherwise" (car cl))))
                       clauses))
         ,(loop :for cl :in clauses
                :when (and (symbolp (car cl))
                           (string-equal "otherwise" (car cl)))
                  :do (return `(t ,@(cdr cl))))))

(defmacro ecase-token (&body clauses)
  (let ((tags (case-token-collect-tags clauses)))
    (declare (ignore tags))
    `(cond ,@(case-token-gen-clauses clauses)
           (t (syntax-error *lookahead*)))))

(defun parse-block ()
  (let* ((filepos (token-filepos *lookahead*))
         (stats (with-accumulate ()
                  (loop
                    (let ((stat (when (not (eof-p))
                                  (parse-stat))))
                      (unless stat
                        (return))
                      (cond ((not (ast-void-p stat))
                             (collect stat)))))))
         (retstat (parse-retstat)))
    (make-ast :block
              filepos
              (if (ast-void-p retstat)
                  stats
                  (nconc stats (list retstat))))))

(defun parse-retstat ()
  (let ((filepos (token-filepos *lookahead*)))
    (if (accept "return")
        (prog1 (make-ast :return
                         filepos
                         (if (exp-start-p)
                             (parse-explist)
                             nil))
          (accept ";"))
        (make-ast :void nil))))

(defun parse-stat ()
  (case-token
    (";"
     (next)
     (make-ast :void nil))
    ("::"
     (parse-label))
    ("break"
     (parse-break))
    ("goto"
     (parse-goto))
    ("do"
     (parse-do))
    ("while"
     (parse-while))
    ("repeat"
     (parse-repeat))
    ("if"
     (parse-if))
    ("for"
     (parse-for))
    ("function"
     (parse-function))
    ("local"
     (parse-local))
    (("word" "(")
     (parse-expr-stat))
    (otherwise nil)))

(defun parse-label ()
  (when (accept "::")
    (let ((name (exact "word")))
      (exact "::")
      (make-ast :label (token-filepos name) (token-value name)))))

(defun parse-break ()
  (let ((filepos (token-filepos *lookahead*)))
    (when (accept "break")
      (make-ast :break filepos))))

(defun parse-goto ()
  (let ((filepos (token-filepos *lookahead*)))
    (when (accept "goto")
      (let ((word (exact "word")))
        (make-ast :goto filepos (token-value word))))))

(defun parse-do ()
  (when (accept "do")
    (prog1 (parse-block)
      (exact "end"))))

(defun parse-do-exact ()
  (exact "do")
  (prog1 (parse-block)
    (exact "end")))

(defun parse-while ()
  (let ((filepos (token-filepos *lookahead*)))
    (when (accept "while")
      (let* ((exp (parse-exp))
             (body (parse-do-exact)))
        (make-ast :while filepos exp body)))))

(defun parse-repeat ()
  (let ((filepos (token-filepos *lookahead*)))
    (when (accept "repeat")
      (let* ((body (parse-block))
             (exp (progn
                    (exact "until")
                    (parse-exp))))
        (make-ast :repeat filepos body exp)))))

(defun parse-if ()
  (labels ((f ()
             (let* ((filepos (token-filepos *lookahead*))
                    (test (parse-exp))
                    (then (progn
                            (exact "then")
                            (parse-block)))
                    (else (cond ((accept "elseif")
                                 (f))
                                (t
                                 (prog1 (if (accept "else")
                                            (parse-block)
                                            (make-ast :void nil))
                                   (exact "end"))))))
               (make-ast :if filepos test then else))))
    (when (accept "if")
      (f))))

(defun parse-for ()
  (let ((filepos (token-filepos *lookahead*)))
    (when (accept "for")
      (let ((namelist (parse-namelist)))
        (cond ((or (and (not (length=1 namelist))
                        (exact "in"))
                   (accept "in"))
               (let* ((explist (parse-explist))
                      (body (parse-do-exact)))
                 (make-ast :generic-for filepos namelist explist body)))
              ((exact "=")
               (let* ((init (parse-exp))
                      (end (progn
                             (exact ",")
                             (parse-exp)))
                      (step (if (accept ",")
                                (parse-exp)
                                (make-ast :number filepos 1))))
                 (make-ast :for
                           filepos
                           (car namelist)
                           init
                           end
                           step
                           (parse-do-exact)))))))))

(defun parse-function ()
  (let ((filepos (token-filepos *lookahead*)))
    (when (accept "function")
      (multiple-value-bind (name method-p)
          (parse-funcname)
        (multiple-value-bind (parlist body)
            (parse-funcbody)
          (when method-p
            (push "self"
                  parlist))
          (make-ast :assign
                    filepos
                    (list name)
                    (list (make-ast :function filepos parlist body))))))))

(defun parse-funcname ()
  (let* ((name (exact "word"))
         (names (loop :while (accept ".")
                      :collect (exact "word")))
         (method-name (when (accept ":")
                        (exact "word"))))
    (let ((var (make-ast :var
                         (token-filepos name)
                         (token-value name)))
          (names (mapcar #'(lambda (name)
                             (make-ast :string
                                       (token-filepos name)
                                       (string-to-lua-string
                                        (token-value name))))
                         (append names
                                 (if method-name
                                     (list method-name))))))
      (values (reduce #'(lambda (x y)
                          (make-ast :index
                                    (token-filepos name)
                                    x
                                    y))
                      (cons var names))
              (if method-name t nil)))))

(defun parse-local ()
  (let ((filepos (token-filepos *lookahead*)))
    (when (accept "local")
      (ecase-token
        ("function" (next) (parse-local-function filepos))
        ("word" (parse-local-vars filepos))))))

(defun parse-local-function (filepos)
  (let* ((token (exact "word"))
         (name (token-value token)))
    (multiple-value-bind (parlist body)
        (parse-funcbody)
      (make-ast :local-function
                filepos
                name
                parlist
                body))))

(defun parse-local-vars (filepos)
  (let* ((namelist (parse-namelist))
         (explist (if (accept "=")
                      (parse-explist)
                      (make-ast :void nil))))
    (make-ast :local
              filepos
              namelist
              explist)))

(defun parse-expr-stat ()
  (let* ((filepos (token-filepos *lookahead*))
         (prefixexp (parse-prefixexp)))
    (case-token
      (("=" ",")
       (unless (member (ast-name prefixexp) '(:var :index))
         (syntax-error *lookahead*))
       (multiple-value-bind (varlist explist) (suffixexp)
         (make-ast :assign
                   filepos
                   (cons prefixexp varlist)
                   explist)))
      (otherwise
       prefixexp))))

(defun suffixexp ()
  (labels ((f (varlist)
             (ecase-token
               ("="
                (next)
                (values (nreverse varlist)
                        (parse-explist)))
               (","
                (next)
                (f (cons (parse-prefixexp) varlist))))))
    (cond ((accept "=")
           (values nil (parse-explist)))
          ((exact ",")
           (f (list (parse-prefixexp)))))))

(defun parse-funcbody ()
  (exact "(")
  (let* ((parlist
           (prog1 (when (match-or "word" "...")
                    (parse-parlist))
             (exact ")")))
         (body
           (prog1 (parse-block)
             (exact "end"))))
    (values parlist body)))

(defun parse-parlist ()
  (cond ((accept "...")
         (list :rest))
        (t
         (cons (token-value (exact "word"))
               (with-accumulate ()
                 (loop
                   (unless (accept ",")
                     (return))
                   (let ((token (accept "word")))
                     (cond (token
                            (collect (token-value token)))
                           (t
                            (when (accept "...")
                              (collect :rest))
                            (return))))))))))

(defun parse-namelist ()
  (mapcar #'token-value
          (cons (exact "word")
                (loop :while (accept ",")
                      :collect (exact "word")))))

(defun parse-explist ()
  (cons (parse-exp)
        (loop :while (accept ",")
              :collect (parse-exp))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *binary-operators*
    '(("or" 1)
      ("and" 2)
      ("<" 3) (">" 3) ("<=" 3) (">=" 3) ("~=" 3) ("==" 3)
      ("|" 4)
      ("~" 5)
      ("&" 6)
      ("<<" 7) (">>" 7)
      (".." 8 t)
      ("+" 9) ("-" 9)
      ("*" 10) ("/" 10) ("//" 10) ("%" 10)
      ("^" 12 t)))
  (defvar *unary-operators*
    '(("not" 11 t) ("#" 11 t) ("-" 11 t) ("~" 11 t))))

(defun operator-left-assoc-p (op)
  (not (third (assoc op *binary-operators*))))

(defun priority (op unary-p)
  (if unary-p
      (second (assoc op *unary-operators* :test #'equal))
      (second (assoc op *binary-operators* :test #'equal))))

(defun exp-start-p ()
  (match-or "nil" "false" "true" "number" "string"
            "..." "function" "word" "(" "{" "not" "#" "-" "~"))

(defun parse-exp-oprand ()
  (ecase-token
    ("nil"
     (next)
     (make-ast :nil (token-filepos *lookahead*)))
    ("false"
     (next)
     (make-ast :false (token-filepos *lookahead*)))
    ("true"
     (next)
     (make-ast :true (token-filepos *lookahead*)))
    ("number"
     (prog1 (make-ast :number
                      (token-filepos *lookahead*)
                      (token-value *lookahead*))
       (next)))
    ("string"
     (prog1 (make-ast :string
                      (token-filepos *lookahead*)
                      (token-value *lookahead*))
       (next)))
    ("..."
     (prog1 (make-ast :rest
                      (token-filepos *lookahead*))
       (next)))
    ("function"
     (parse-functiondef))
    (("word" "(")
     (parse-prefixexp))
    ("{"
     (parse-tableconstructor))))

(defun parse-exp ()
  (let ((rpn)
        (stack))
    ;; rpn   ::= ((:oprand exp . nil) ...)
    ;;         | ((:operator op-token . unary-p) ...)
    ;; stack ::= ((op-token . unary-p) ...)
    (loop
      (when (match-or "-" "not" "#" "~")
        (push (list (next) t) stack))
      (let ((oprand (parse-exp-oprand)))
        (push (list :oprand oprand) rpn))
      (case-token
        (#.(mapcar #'car *binary-operators*)
           (let* ((token (next))
                  (op1 (token-value token)))
             (when (operator-left-assoc-p op1)
               (loop :while (and (not (null stack))
                                 (destructuring-bind (op2-token . unary-p)
                                     (car stack)
                                   (<= (priority op1 nil)
                                       (priority (token-value op2-token)
                                                 unary-p))))
                     :do (push (cons :operator (pop stack))
                               rpn)))
             (push (cons token nil) stack)))
        (otherwise
         (return))))
    (dolist (elt stack)
      (push (cons :operator elt) rpn))
    (rpn-to-ast (nreverse rpn))))

(defun rpn-to-ast (rpn)
  (let ((stack))
    (dolist (elt rpn)
      (destructuring-ecase elt
        ((:oprand exp)
         (push exp stack))
        ((:operator op-token . unary-p)
         (push (if unary-p
                   (make-ast :unary-op
                             (token-filepos op-token)
                             (token-value op-token)
                             (pop stack))
                   (let ((right (pop stack))
                         (left (pop stack)))
                     (make-ast :binary-op
                               (token-filepos op-token)
                               (token-value op-token)
                               left
                               right)))
               stack))))
    (assert (length=1 stack))
    (car stack)))

(defun parse-functiondef ()
  (let ((filepos (token-filepos (exact "function"))))
    (multiple-value-bind (parlist body)
        (parse-funcbody)
      (make-ast :function
                filepos
                parlist
                body))))

(defun parse-prefixexp ()
  (ecase-token
    ("word"
     (let ((filepos (token-filepos *lookahead*))
           (name (token-value *lookahead*)))
       (next)
       (parse-prefixexp-tail
        (make-ast :var filepos name))))
    ("("
     (next)
     (let* ((filepos (token-filepos *lookahead*))
            (exp (parse-exp)))
       (exact ")")
       (parse-prefixexp-tail (make-ast :paren filepos exp))))))

(defun parse-prefixexp-tail (x)
  (case-token
    ("["
     (let ((filepos (token-filepos (next))))
       (let ((exp (parse-exp)))
         (exact "]")
         (parse-prefixexp-tail
          (make-ast :index
                    filepos
                    x
                    exp)))))
    ("."
     (let* ((filepos (token-filepos (next)))
            (name (exact "word")))
       (parse-prefixexp-tail
        (make-ast :index
                  filepos
                  x
                  (make-ast :string
                            (token-filepos name)
                            (string-to-lua-string
                             (token-value name)))))))
    (("(" "{" "string")
     (let ((filepos (token-filepos *lookahead*)))
       (parse-prefixexp-tail
        (make-ast :call-function
                  filepos
                  x
                  (parse-args)))))
    (":"
     (let* ((filepos (token-filepos (next)))
            (name (exact "word"))
            (args (parse-args)))
       (parse-prefixexp-tail
        (make-ast :call-method
                  filepos
                  x
                  (token-value name)
                  args))))
    (otherwise x)))

(defun parse-args ()
  (ecase-token
    ("("
     (next)
     (prog1 (if (exp-start-p)
                (parse-explist)
                nil)
       (exact ")")))
    ("{"
     (list (parse-tableconstructor)))
    ("string"
     (let ((filepos (token-filepos *lookahead*))
           (value (token-value *lookahead*)))
       (next)
       (list (make-ast :string filepos value))))))

(defun parse-tableconstructor ()
  (let ((filepos (token-filepos *lookahead*)))
    (exact "{")
    (multiple-value-bind (field-sequence field-pairs)
        (parse-fieldlist)
      (exact "}")
      (make-ast :tableconstructor
                filepos
                (or field-sequence nil)
                (or field-pairs nil)))))

(defun parse-fieldlist ()
  (cond
    ((not (field-start-p))
     nil)
    (t
     (let ((field-sequence)
           (field-pairs))
       (loop
         (multiple-value-bind (match-p field serial-p)
             (parse-field)
           (cond ((not match-p)
                  (return))
                 (serial-p
                  (push field field-sequence))
                 (t
                  (push field field-pairs))))
         (if (match-or "," ";")
             (next)
             (return)))
       (values (nreverse field-sequence)
               (nreverse field-pairs))))))

(defun field-start-p ()
  (or (match-p "[")
      (exp-start-p)))

(defun parse-field ()
  (cond ((accept "[")
         (let ((key (parse-exp)))
           (exact "]")
           (exact "=")
           (let ((value (parse-exp)))
             (values t
                     (list key value)
                     nil))))
        ((exp-start-p)
         (let ((name (accept "word")))
           (cond
             ((and name (accept "="))
              (let* ((exp (parse-exp))
                     (key (make-ast :var
                                    (token-filepos name)
                                    (token-value name))))
                (values t
                        (list key exp)
                        nil)))
             (t
              (when name
                (pushback name))
              (values t (parse-exp) t)))))))

(defun parse (*lexer*)
  (let ((*lookahead-undo-stack*)
        (*lookahead*))
    (next)
    (parse-block)))

(defun parse-from-string (string)
  (with-lexer-from-string (lexer string)
    (parse lexer)))
