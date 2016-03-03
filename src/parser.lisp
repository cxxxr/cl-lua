(in-package :cl-user)
(defpackage :cl-lua.parser
  (:use
   :cl
   :cl-lua.util
   :cl-lua.lexer
   :cl-lua.error
   :cl-lua.token
   :cl-lua.ast
   :alexandria)
  (:export
   :parser-error
   :parse
   :parse-from-string))
(in-package :cl-lua.parser)

(defvar *lexer*)
(defvar *lookahead*)
(defvar *lookahead-undo-stack*)

(defun parser-error (token expected-tag actual-tag)
  (error 'parser-error
         :token token
         :expected-tag expected-tag
         :actual-tag actual-tag))

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
      (parser-error *lookahead*
                    expected-tag
                    tag))
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
    `(cond ,@(case-token-gen-clauses clauses)
           (t (parser-error *lookahead*
                            ',tags
                            (token-tag *lookahead*))))))

(defun parse-block ()
  (let* ((linum (token-linum *lookahead*))
         (stats (flatten-stats
                 (loop :for stat := (when (not (eof-p))
                                      (parse-stat))
                       :while stat
                       :collect stat)))
         (retstat (parse-retstat)))
    (make-ast :block linum stats retstat)))

(defun flatten-stats (stats)
  (loop :for stat :in stats
        :if (consp (car stat))
          :append stat
        :else
          :if (not (ast-void-p stat))
            :collect stat))

(defun parse-retstat ()
  (let ((linum (token-linum *lookahead*)))
    (if (accept "return")
        (prog1 (make-ast :return
                         linum
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
      (make-ast :label (token-linum name) (token-value name)))))

(defun parse-break ()
  (let ((linum (token-linum *lookahead*)))
    (when (accept "break")
      (make-ast :break linum))))

(defun parse-goto ()
  (let ((linum (token-linum *lookahead*)))
    (when (accept "goto")
      (let ((word (exact "word")))
        (make-ast :goto linum (token-value word))))))

(defun parse-do ()
  (when (accept "do")
    (prog1 (parse-block)
      (exact "end"))))

(defun parse-do-exact ()
  (exact "do")
  (prog1 (parse-block)
    (exact "end")))

(defun parse-while ()
  (let ((linum (token-linum *lookahead*)))
    (when (accept "while")
      (let* ((exp (parse-exp))
             (body (parse-do-exact)))
        (make-ast :while linum exp body)))))

(defun parse-repeat ()
  (let ((linum (token-linum *lookahead*)))
    (when (accept "repeat")
      (let* ((body (parse-block))
             (exp (progn
                    (exact "until")
                    (parse-exp))))
        (make-ast :repeat linum body exp)))))

(defun parse-if ()
  (labels ((f ()
             (let* ((linum (token-linum *lookahead*))
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
               (make-ast :if linum test then else))))
    (when (accept "if")
      (f))))

(defun parse-for ()
  (let ((linum (token-linum *lookahead*)))
    (when (accept "for")
      (let ((namelist (parse-namelist)))
        (cond ((or (and (not (length=1 namelist))
                        (exact "in"))
                   (accept "in"))
               (let* ((explist (parse-explist))
                      (body (parse-do-exact)))
                 (make-ast :generic-for linum namelist explist body)))
              ((exact "=")
               (let* ((init (parse-exp))
                      (end (progn
                             (exact ",")
                             (parse-exp)))
                      (step (if (accept ",")
                                (parse-exp)
                                (make-ast :number linum 1))))
                 (make-ast :for
                           linum
                           (car namelist)
                           init
                           end
                           step
                           (parse-do-exact)))))))))

(defun parse-function ()
  (let ((linum (token-linum *lookahead*)))
    (when (accept "function")
      (multiple-value-bind (name method-p)
          (parse-funcname)
        (multiple-value-bind (parlist body)
            (parse-funcbody)
          (when method-p
            (push "self"
                  parlist))
          (make-ast :assign
                    linum
                    (list name)
                    (list (make-ast :function linum parlist body))))))))

(defun parse-funcname ()
  (let* ((name (exact "word"))
         (names (loop :while (accept ".")
                      :collect (exact "word")))
         (method-name (when (accept ":")
                        (exact "word"))))
    (let ((var (make-ast :var
                         (token-linum name)
                         (token-value name)))
          (names (mapcar #'(lambda (name)
                             (make-ast :string
                                       (token-linum name)
                                       (string-to-bytes (token-value name))))
                         (append names
                                 (if method-name
                                     (list method-name))))))
      (values (reduce #'(lambda (x y)
                          (make-ast :refer-table
                                    (token-linum name)
                                    x
                                    y))
                      (cons var names))
              (if method-name t nil)))))

(defun parse-local ()
  (let ((linum (token-linum *lookahead*)))
    (when (accept "local")
      (ecase-token
        ("function" (next) (parse-local-function linum))
        ("word" (parse-local-vars linum))))))

(defun parse-local-function (linum)
  (let* ((token (exact "word"))
         (var (token-value token)))
    (multiple-value-bind (parlist body)
        (parse-funcbody)
      (list
       (make-ast :local
                 linum
                 (list var)
                 (make-ast :void nil))
       (make-ast :assign
                 linum
                 (list var)
                 (list (make-ast :function
                                 linum
                                 parlist
                                 body)))))))

(defun parse-local-vars (linum)
  (let* ((namelist (parse-namelist))
         (explist (if (accept "=")
                      (parse-explist)
                      (make-ast :void nil))))
    (make-ast :local
              linum
              namelist
              explist)))

(defun parse-expr-stat ()
  (let* ((linum (token-linum *lookahead*))
         (prefixexp (parse-prefixexp)))
    (multiple-value-bind (match-p varlist explist)
        (suffixexp)
      (if match-p
          (make-ast :assign
                    linum
                    (cons prefixexp varlist)
                    explist)
          prefixexp))))

(defun suffixexp ()
  (labels ((f (varlist)
             (ecase-token
               ("=" (values t
                            (nreverse varlist)
                            (parse-explist)))
               ("," (f (cons (parse-prefixexp) varlist))))))
    (cond ((accept "=")
           (values t nil (parse-explist)))
          ((accept ",")
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
     (make-ast :nil (token-linum *lookahead*)))
    ("false"
     (next)
     (make-ast :false (token-linum *lookahead*)))
    ("true"
     (next)
     (make-ast :true (token-linum *lookahead*)))
    ("number"
     (prog1 (make-ast :number
                      (token-linum *lookahead*)
                      (token-value *lookahead*))
       (next)))
    ("string"
     (prog1 (make-ast :string
                      (token-linum *lookahead*)
                      (token-value *lookahead*))
       (next)))
    ("..."
     (prog1 (make-ast :rest
                      (token-linum *lookahead*))
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
                             (token-linum op-token)
                             (token-value op-token)
                             (pop stack))
                   (let ((right (pop stack))
                         (left (pop stack)))
                     (make-ast :binary-op
                               (token-linum op-token)
                               (token-value op-token)
                               left
                               right)))
               stack))))
    (assert (length=1 stack))
    (car stack)))

(defun parse-functiondef ()
  (let ((linum (token-linum (exact "function"))))
    (multiple-value-bind (parlist body)
        (parse-funcbody)
      (make-ast :function
                linum
                parlist
                body))))

(defun parse-prefixexp ()
  (ecase-token
    ("word"
     (let ((linum (token-linum *lookahead*))
           (name (token-value *lookahead*)))
       (next)
       (parse-prefixexp-tail
        (make-ast :var linum name))))
    ("("
     (next)
     (let ((exp (parse-exp)))
       (exact ")")
       (parse-prefixexp-tail exp)))))

(defun parse-prefixexp-tail (x)
  (case-token
    ("["
     (let ((linum (token-linum (next))))
       (let ((exp (parse-exp)))
         (exact "]")
         (parse-prefixexp-tail
          (make-ast :refer-table
                    linum
                    x
                    exp)))))
    ("."
     (let* ((linum (token-linum (next)))
            (name (exact "word")))
       (parse-prefixexp-tail
        (make-ast :refer-table
                  linum
                  x
                  (make-ast :string
                            (token-linum name)
                            (string-to-bytes
                             (token-value name)))))))
    (("(" "{" "string")
     (let ((linum (token-linum *lookahead*)))
       (parse-prefixexp-tail
        (make-ast :call-function
                  linum
                  x
                  (parse-args)))))
    (":"
     (let* ((linum (token-linum (next)))
            (name (exact "word"))
            (args (parse-args)))
       (parse-prefixexp-tail
        (make-ast :call-method
                  linum
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
     (let ((linum (token-linum *lookahead*))
           (value (token-value *lookahead*)))
       (next)
       (list (make-ast :string linum value))))))

(defun parse-tableconstructor ()
  (let ((linum (token-linum *lookahead*)))
    (exact "{")
    (multiple-value-bind (fieldarray fieldpairs)
        (parse-fieldlist)
      (exact "}")
      (make-ast :tableconstructor
                linum
                (or fieldarray (make-ast :void nil))
                (or fieldpairs (make-ast :void nil))))))

(defun parse-fieldlist ()
  (cond
    ((not (field-start-p))
     nil)
    (t
     (let ((fieldarray)
           (fieldpairs))
       (loop
         (multiple-value-bind (match-p field serial-p)
             (parse-field)
           (cond ((not match-p)
                  (return))
                 (serial-p
                  (push field fieldarray))
                 (t
                  (push field fieldpairs))))
         (if (match-or "," ";")
             (next)
             (return)))
       (values (nreverse fieldarray)
               (nreverse fieldpairs))))))

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
                                    (token-linum name)
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
