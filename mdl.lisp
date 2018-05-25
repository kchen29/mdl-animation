;;;;compiler for MDL. Refer to MDL.spec

(defvar commands '(push pop
                   move scale rotate
                   sphere torus box line mesh
                   basename set save_knobs tween frames vary setknobs
                   light ambient constants shading
                   save_coord_system camera save generate_rayfiles focal display))

(defun classifier (str stream)
  "Classifies each substring as a token."
  (cond
    ((member (make-symbol (string-upcase str)) commands :test #'name=) (make-symbol (string-upcase str)))
    ((char= #\/ (char str 0)) (read-line stream) nil)
    ((char= #\: (char str 0)) (let ((x (make-symbol "COLON")))
                                (setf (symbol-value x) (subseq str 1))
                                x))
    ((alpha-char-p (char str 0)) (make-symbol "SYMBOL"))
    ((numberp (read-from-string str)) (let ((x (make-symbol "NUMBER")))
                                        (setf (symbol-value x) (read-from-string str))
                                        x))
    (t (error "~a does not indicate a token." str))))

(defmacro add-op (&body body)
  "Adds an operation to the operation list. BODY is the body of the lambda."
  `(push (lambda () ,@body) ops))

(defmacro with-knob ((var sym) &body body)
  "Lets VAR be the value of SYM in the symbol table."
  `(let ((,var (gethash ,sym symbol-table 1)))
     ,@body))

(defun parse-mdl (token-list)
  "Parses TOKEN-LIST following the mdl grammar. Returns the relevant data as a values."
  (let ((stack (list (make-transform-matrix)))
        (edges (make-matrix))
        (polygons (make-matrix)) 
        basename
        frames
        (symbol-table (make-hash-table :test #'equal))
        knob-ops
        ops)
    (labels ((post-add-lines ()
               (matrix-multiply (car stack) edges)
               (draw-lines edges '(255 0 255))
               (clear-matrix edges))
             (post-add-polygons ()
               (matrix-multiply (car stack) polygons)
               (draw-polygons polygons)
               (clear-matrix polygons))
             (update-current-stack (transform)
               (push (matrix-multiply (pop stack) transform) stack)))
      (do ()
          ((not token-list))
        (parse token-list
          (push
           (add-op
             (push (copy-matrix (car stack)) stack)))
          (pop
           (add-op
             (pop stack)))

          ((move number number number (&opt symbol))
           (if a4
               (add-op (with-knob (v a4)
                         (update-current-stack (make-translate (* a1 v)
                                                               (* a2 v)
                                                               (* a3 v)))))
               (add-op (update-current-stack (make-translate a1 a2 a3)))))
          ((scale number number number (&opt symbol))
           (if a4
               (add-op (with-knob (v a4)
                         (update-current-stack (make-scale (* a1 v)
                                                           (* a2 v)
                                                           (* a3 v)))))
               (add-op (update-current-stack (make-scale a1 a2 a3)))))
          ((rotate symbol number (&opt symbol))
           (if a3
               (add-op (with-knob (v a3)
                         (update-current-stack (make-rotate (concat-symbol a1)
                                                            (* a2 v)))))
               (add-op (update-current-stack (make-rotate (concat-symbol a1)
                                                          a2)))))
          ((sphere (&opt symbol) number number number number (&opt symbol))
           (add-op
             (add-sphere polygons 10 a2 a3 a4 a5)
             (post-add-polygons)))
          ((torus (&opt symbol) number number number number number (&opt symbol))
           (add-op
             (add-torus polygons 10 a2 a3 a4 a5 a6)
             (post-add-polygons)))
          ((box (&opt symbol) number number number number number number (&opt symbol))
           (add-op
             (add-box polygons a2 a3 a4 a5 a6 a7)
             (post-add-polygons)))
          ((line (&opt symbol) number number number (&opt symbol) number number number (&opt symbol))
           (add-op
             (add-edge edges a2 a3 a4 a6 a7 a8)
             (post-add-lines)))
          ((mesh (&opt symbol) colon (&opt symbol)))

          ((basename symbol)
           (setf basename a1))
          ((set symbol number))
          ((save_knobs symbol))
          ((tween number number symbol symbol))
          ((frames number)
           (setf frames a1))
          ((vary symbol number number number number)
           (let ((dif (diff-quot a5 a4 a3 a2)))
             (push (lambda (frame)
                     (cond
                       ((= frame a2)
                        (setf (gethash a1 symbol-table) a4))
                       ((<= a2 frame a3)
                        (incf (gethash a1 symbol-table) dif))))
                   knob-ops)))
          ((setknobs number))

          ((light symbol number number number number number number))
          ((ambient number number number))
          ((constants symbol number number number number number number number number number (&opt number) (&opt number) (&opt number)))
          ((shading symbol))

          ((save_coord_system symbol))
          ((camera number number number number number number))
          ((save symbol)
           (add-op
             (save a1)))
          (generate_rayfiles)
          ((focal number))
          (display
           (add-op
             (display))))))
    (values basename frames (nreverse knob-ops) (nreverse ops)
            (lambda () (setf stack (list (make-transform-matrix)))))))

(defun compile-mdl (file)
  "Compiles FILE to an image."
  (multiple-value-bind (basename frames knob-ops ops clear-stack)
      (parse-mdl (lexify file #'classifier))
    (cond
      (frames
       (let ((frames-digs (integer-digits (1- frames))))
         (dotimes (frame frames)
           (dolist (op knob-ops)
             (funcall op frame))
           (format t "Frame ~a~%" frame)
           (funcall clear-stack)
           (clear-screen)
           (dolist (op ops)
             (funcall op))
           (save-frame basename frame frames-digs)))
       (make-animation basename))
      (t (dolist (op ops)
           (funcall op))))))
