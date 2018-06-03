;;;; Matrices and transformations.

(def-my-class (matrix :conc-name m-)
    ((rows 4)
     (cols 4)
     (last-col 0)
     (array (make-array (list rows cols) :adjustable t)))

  (defmacro mref (matrix x y)
    "Accesses array of MATRIX at X and Y."
    `(aref (m-array ,matrix) ,x ,y))

  (defun copy-matrix (matrix)
    "Copies a matrix."
    (make-matrix :rows rows :cols cols :last-col last-col :array (copy-array array)))

  (defun clear-matrix (matrix)
    "Clears MATRIX."
    (setf last-col 0))

  (defun adjust-matrix (matrix new-rows new-cols)
    "Adjusts MATRIX to ROWS and COLS. Keeps last-col."
    (adjust-array array (list new-rows new-cols))
    (setf rows new-rows)
    (setf cols new-cols))

  (defun to-identity (matrix)
    "Turns MATRIX into an identity matrix. Returns the matrix."
    (dotimes (x rows matrix)
      (dotimes (y last-col)
        (if (= x y)
            (setf (aref array x y) 1)
            (setf (aref array x y) 0)))))

  ;;closure, for fun
  (let ((temp (make-array 4)))
    (defun matrix-multiply (matrix m2)
      "A specific matrix multiplication routine. MATRIX is square, 4 by 4
       Multiplies MATRIX with M2. Modifies M2 to hold the result. Returns M2."
      (declare (optimize (speed 3) (safety 0)))
      (let ((array array)
            (rows rows)
            (cols cols)
            (last-col2 (m-last-col m2))
            (array2 (m-array m2)))
        (declare (type fixnum rows cols last-col2))
        (dotimes (col last-col2 m2)
          (dotimes (i rows)
            (setf (svref temp i) (aref array2 i col)))
          (dotimes (row rows)
            (setf (aref array2 row col)
                  (loop for i below cols
                        sum (* (aref array row i) (svref temp i))))))))))

;;;transformations
(defun make-transform-matrix ()
  (to-identity (make-matrix :last-col 4)))

(defmacro deftransform (transform-name args &body body)
  "Defuns make-transform given TRANSFORM-NAME, using ARGS and BODY.
   Requires docstring as part of BODY.
   Also defuns transform, applying make-transform to another matrix."
  (let ((make-symbol (concat-symbol "MAKE-" transform-name)))
    `(progn
       (defun ,make-symbol ,args
         ,(concatenate 'string "Makes a matrix that " (pop body))
         (let ((transform (make-transform-matrix)))
           ,@body
           transform))
       (defun ,transform-name (matrix ,@args)
         ,(concat-string "Applies make-" transform-name " to MATRIX.")
         (matrix-multiply (,make-symbol ,@args) matrix)))))

(deftransform translate (delx dely delz)
  "translates by DELX, DELY, and DELZ."
  (setf (mref transform 0 3) delx
        (mref transform 1 3) dely
        (mref transform 2 3) delz))

(deftransform scale (x-scale y-scale z-scale)
  "scales x by X-SCALE, y by Y-SCALE, and z by Z-SCALE."
  (setf (mref transform 0 0) x-scale
        (mref transform 1 1) y-scale
        (mref transform 2 2) z-scale))

(defmacro defrotation (rotate-axis axis-0 axis-1)
  "Defines a rotation around ROTATE-AXIS. AXIS-0 and AXIS-1 mark the value of the axes,
   where x corresponds to 0, y 1, and z 2. Rotates from AXIS-0 to AXIS-1."
  `(deftransform ,(concat-symbol "ROTATE-" rotate-axis) (degrees)
     ,(concat-string "rotates by DEGREES counter-clockwise using "
                     rotate-axis " as the axis.")
     (let ((radians (/ (* degrees pi) 180)))
       (setf (mref transform ,axis-0 ,axis-0) (cos radians)
             (mref transform ,axis-0 ,axis-1) (- 0 (sin radians))
             (mref transform ,axis-1 ,axis-0) (sin radians)
             (mref transform ,axis-1 ,axis-1) (cos radians)))))

(defrotation z 0 1)
(defrotation x 1 2)
(defrotation y 2 0)

(defun make-rotate (axis degrees)
  "Makes a matrix that rotates by DEGREES counter-clockwise using AXIS."
  (case axis
    (x (make-rotate-x degrees))
    (y (make-rotate-y degrees))
    (z (make-rotate-z degrees))
    (otherwise (format t "Unknown axis: ~a~%" axis))))

(defun rotate (matrix axis degrees)
  "Rotate MATRIX by the rotation matrix with AXIS by DEGREES."
  (matrix-multiply (make-rotate axis degrees) matrix))
