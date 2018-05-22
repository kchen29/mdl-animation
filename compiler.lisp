;;;;compiler compiler

;;;lexer
(defun lexify (file classifier)
  "Given CLASSIFIER, classify each token in FILE, producing a token list.
   CLASSIFIER should return fresh uninterned symbols.
   If NIL is returned, the token is not added.
   If the returned symbol is not bound, give it the value of the substring."
  (with-open-file (stream file)
    (collect-to
     (do ((sub (read-token stream) (read-token stream)))
         ((string= "" sub))
       (let ((sym (funcall classifier sub stream)))
         (when sym
           (unless (boundp sym)
             (setf (symbol-value sym) sub))
           (collect sym)))))))

(defun read-token (stream)
  "Reads a token from STREAM."
  (with-output-to-string (s-stream)
    ;;remove all whitespace
    (do-stream (char stream) ((not (whitespace-p char))
                              (when (characterp char)
                                (unread-char char stream))))
    ;;then read token
    (do-stream (char stream) ((whitespace-p char)
                              (when (characterp char)
                                (unread-char char stream)))
      (princ char s-stream))))

;;;primitive parser
(defun opt-p (pattern)
  "Returns if PATTERN is optional."
  (and (listp pattern) (eq (car pattern) '&opt)))

(defmacro parse-text (tokens-var patterns)
  "Tests if TOKENS-VAR matches PATTERNS in MATCH-FORM."
  (let ((temp (gensym)))
    `(let ((,temp ,tokens-var))
       (and ,@(loop for pattern in patterns
                    collect (if (opt-p pattern)
                                `(if (name= ',(cadr pattern) (car ,temp))
                                     (pop ,temp)
                                     t)
                                `(name= ',pattern (pop ,temp))))))))

(defmacro parse-actions (tokens-var (patterns &rest actions))
  "Anaphoric macro for ACTIONS."
  `(let ,(loop for pattern in patterns
               for i = 0 then (1+ i)
               collect `(,(concat-symbol 'a i)
                         ,(if (opt-p pattern)
                              `(if (name= ',(cadr pattern) (car ,tokens-var))
                                   (symbol-value (pop ,tokens-var))
                                   nil)
                              `(symbol-value (pop ,tokens-var)))))
     (declare (ignorable ,@(loop for pattern in patterns
                                 for i = 0 then (1+ i)
                                 collect (concat-symbol 'a i))))
     ,@actions))

(defmacro parse (tokens-var &body grammar)
  "Generates a parser for TOKENS-VAR following GRAMMAR.
   Each form in GRAMMAR is of the form (PATTERNS &rest ACTIONS).
     PATTERNS can be an atom or a list. If it is an atom, it is equal to a singleton list.
     PATTERNS is matched if the token-list matches each item sequentially.
     Each form in PATTERNS is either a token or a list.
       If the form is a token then the token-list should have that token as the next token
       If the form is a list and has &opt as the first argument then that token is optional.
     Each ACTION in ACTIONS can use the value of each token by using the symbols 
     A0, A1, A2, ... respectively indicating the index of each token in the pattern."
  `(cond
     ,@(loop for match-form in grammar
             if (atom (car match-form))
               do (setf (car match-form) (list (car match-form)))
             collect `((parse-text ,tokens-var ,(car match-form))
                       (parse-actions ,tokens-var ,match-form)))
     (t (error "Parser error. Token list: ~a" ,tokens-var))))
