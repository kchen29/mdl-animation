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

(defun compile-mdl (file)
  "Compiles FILE to an image."
  (let ((stack (list (make-transform-matrix)))
        (edges (make-matrix))
        (polygons (make-matrix)))
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
      (do ((token-list (lexify file #'classifier)))
          ((not token-list))
        (parse token-list
          (push
           (push (copy-matrix (car stack)) stack))
          (pop
           (pop stack))

          ((move number number number (&opt symbol))
           (update-current-stack (make-translate a1 a2 a3)))
          ((scale number number number (&opt symbol))
           (update-current-stack (make-scale a1 a2 a3)))
          ((rotate symbol number (&opt symbol))
           (update-current-stack (make-rotate (concat-symbol a1) a2)))

          ((sphere (&opt symbol) number number number number (&opt symbol))
           (add-sphere polygons 10 a2 a3 a4 a5)
           (post-add-polygons))
          ((torus (&opt symbol) number number number number number (&opt symbol))
           (add-torus polygons 10 a2 a3 a4 a5 a6)
           (post-add-polygons))
          ((box (&opt symbol) number number number number number number (&opt symbol))
           (add-box polygons a2 a3 a4 a5 a6 a7)
           (post-add-polygons))
          ((line (&opt symbol) number number number (&opt symbol) number number number (&opt symbol))
           (add-edge edges a2 a3 a4 a6 a7 a8)
           (post-add-lines))
          ((mesh (&opt symbol) colon (&opt symbol)))

          ((basename symbol))
          ((set symbol number))
          ((save_knobs symbol))
          ((tween number number symbol symbol))
          ((frames number))
          ((vary symbol number number number number))
          ((setknobs number))

          ((light symbol number number number number number number))
          ((ambient number number number))
          ((constants symbol number number number number number number number number number (&opt number) (&opt number) (&opt number)))
          ((shading symbol))

          ((save_coord_system symbol))
          ((camera number number number number number number))
          ((save symbol)
           (save a1))
          (generate_rayfiles)
          ((focal number))
          (display
           (display t)))))))
