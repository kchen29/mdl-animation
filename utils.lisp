;;;;General utility functions/macros.

;;;macros
;;general macros
(defmacro macrolet-helper (&body body)
  "Defines a single macrolet with BODY and evaluates it."
  (let ((temp (gensym)))
    `(macrolet ((,temp () ,@body))
       (,temp))))

;;it's possible to allow the user to choose whether to explicitly name the variable or not
;;not implemented though
(defmacro collect-to (&body body)
  "Defines a flet to collect objects. The reverse of the collected is returned."
  (let ((var (gensym)))
    `(let (,var)
       (flet ((collect (obj) (push obj ,var)))
         ,@body
         (nreverse ,var)))))

(defmacro nested-loops (loops &body body)
  "Creates nested loops."
  (let ((head `(progn ,@body)))
    (dolist (form (nreverse loops) head)
      (setf head (list (if (listp (cadr form)) 'dolist 'dotimes)
                       form head)))))

;;the iteration value for each of the loops should be a list or positive integer
(defmacro generate (loops &body bases)
  "Generates nested loops and collects BASES."
  `(collect-to
     (nested-loops ,loops ,@(mapcar (lambda (x) `(collect ,x)) bases))))

;;object
(defmacro def-class (name-args args &rest rest)
  "Defines a class. NAME-ARGS can be a symbol or a list (with :conc-name).
   ARGS defines the variables and their initial values.
   REST is wrapped with a with-args, with all the args, using the NAME of the class.
   Provides accessor functions for each arg."
  (let ((temp (gensym))
        (name (if (listp name-args)
                  (pop name-args)
                  name-args))
        (conc (if (listp name-args)
                  (getf name-args :conc-name)
                  (concat-symbol name-args "-")))
        (arg-names (mapcar #'car args)))
    `(progn
       (defun ,(concat-symbol "make-" name) (&key ,@args)
         (let ((,temp (make-hash-table :size ,(length arg-names))))
           (setf ,@(loop for arg in arg-names
                         collect `(gethash ,(make-keyword arg) ,temp)
                         collect arg))
           ,temp))
       ,@(mapcar (lambda (x) `(defmacro ,(concat-symbol conc x) (,name)
                                `(gethash ,,(make-keyword x) ,,name)))
                 arg-names)
       (with-args ,arg-names ,name
         ,@rest))))

(defmacro with-args (args obj &body body)
  "Wraps BODY with a symbol-macrolet, allowing each arg in OBJ to be treated as variables.
   Each arg can be a symbol or a list, where the first element is the 'variable'."
  `(symbol-macrolet
       ,(mapcar (lambda (x) (if (atom x)
                                `(,x (gethash ,(make-keyword x) ,obj))
                                `(,(pop x) (gethash ,(make-keyword (car x)) ,obj))))
                args)
     ,@body))

;;control constructs
(defmacro do-step-max ((var step max) &body body)
  "Iterate for VAR STEP times from 0 to MAX, inclusive."
  (let ((temp (gensym)))
    `(loop for ,temp upto ,step
           for ,var = (* ,max (/ ,temp ,step))
           do ,@body)))

(defmacro do-step ((var count step) &body body)
  "Iterate for VAR from 0 below COUNT with STEP interval."
  `(do ((,var 0 (+ ,step ,var)))
       ((>= ,var ,count))
     ,@body))

(defmacro do-pairwise ((var1 var2 list) &body body)
  "Iterates over LIST pairwise, with VAR1 and VAR2."
  (let ((list-move (gensym)))
    `(do* ((,list-move ,list (cdr ,list-move))
           (,var1 (car ,list) (car ,list-move)))
          ((not ,list-move))
       (dolist (,var2 (cdr ,list-move))
         ,@body))))

(defmacro do-stream ((var stream) (test &optional (result nil result-supplied-p)) &body body)
  "Concise way to iterate over STREAM. Iterates over each character; stops when eof is found or
   TEST is true, and returns RESULT. Performs BODY after each time VAR is set."
  `(do ((,var (read-char ,stream nil nil)
              (read-char ,stream nil nil)))
       ,(if result-supplied-p
            `((or (null ,var) ,test) ,result)
            `((or (null ,var) ,test)))
     ,@body))

;;switch
(defmacro switch (value test &body cases)
  "Macro for switch-case statements.
   TESTs VALUE with the first element in each case of CASES.
   If otherwise is the first element, then it acts as the default case."
  `(cond
     ,@(loop for case in cases
             for test-value = (first case)
             for return-value = (rest case)
             if (eql 'otherwise test-value)
               collect `(t ,@return-value)
             else
               collect `((funcall ,test ,value ,test-value) ,@return-value))))

;;symbol manipulation
(defmacro roundify (&rest args)
  "Rounds each symbol."
  `(setf ,@(loop for arg in args
                 collect arg
                 collect `(round ,arg))))

(defmacro sortify (index &rest cases)
  "Sorts each case via when and rotatef. Checks the symbol at INDEX.
   Has the first case be least, and last case be greatest."
  `(progn
     ,@(collect-to
         (do-pairwise (case1 case2 cases)
           (collect `(when (> ,(nth index case1) ,(nth index case2))
                       ,@(loop for x in case1
                               for y in case2
                               collect `(rotatef ,x ,y))))))))

;;;functions
;;numeric
(defun evaluate-polynomial (x &rest coefficients)
  "Evaluates a polynomial in X with COEFFICIENTS. Starts from the least power (x^0) and
   increases with each coefficient."
  (loop for coeff in coefficients
        for product = 1 then (* x product)
        sum (* coeff product)))

(defun fround-to (number decimals)
  "Rounds NUMBER to DECIMALS places."
  (/ (fround (* number (expt 10 decimals)))
     (expt 10 decimals)))

(defun diff-quot (a b c d)
  "Calculates the difference quotient of A minus B divided by C minus D."
  (/ (- a b) (- c d)))

;;array
(defun copy-array (array)
  "Copies an array."
  (let ((dims (array-dimensions array)))
    (adjust-array (make-array dims :displaced-to array) dims)))

;;symbols
(defun concat-symbol (&rest args)
  "Takes symbols and strings to form a new symbol."
  (intern (string-upcase (apply #'concat-string args))))

(defun concat-string (&rest args)
  "Takes symbols and strings to form a string."
  (string-downcase (with-output-to-string (s)
                     (dolist (a args) (princ a s)))))

(defun make-keyword (sym)
  "Returns SYM as a keyword."
  (intern (string sym) 'keyword))

(defun name= (sym1 sym2)
  "Returns if SYM1 and SYM2 have string= symbol names."
  (string= (symbol-name sym1) (symbol-name sym2)))

;;characters
(defun whitespace-p (char)
  "Returns true if CHAR is a whitespace character."
  (or (not (graphic-char-p char)) (char= char #\Space)))


