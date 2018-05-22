;;;; math functions for vectors and lighting.

;;closure, for fun
(let ((temp1 (make-array 3))
      (temp2 (make-array 3)))
  (defun normal (polygons index)
    "Returns the normal of the surface defined by POLYGONS at INDEX."
    (dotimes (x 3)
      (setf (svref temp1 x) (- (mref polygons x index)
                               (mref polygons x (1+ index)))
            (svref temp2 x) (- (mref polygons x index)
                               (mref polygons x (+ 2 index)))))
    (cross-product temp1 temp2)))

(defun cross-product (v1 v2)
  "Returns the cross-product of V1 and V2."
  (labels ((prod (a b)
             (* (svref v1 a)
                (svref v2 b)))
           (dif (a b)
             (- (prod a b)
                (prod b a))))
    (vector (dif 1 2)
            (dif 2 0)
            (dif 0 1))))

;;;treating vectors as lists
(defun dot (v1 v2)
  "Dots V1 and V2."
  (loop for x in v1
        for y in v2
        sum (* x y)))

(defun magnitude (vector)
  "Calculates the magnitude of the vector."
  (sqrt (loop for x in vector
              sum (* x x))))

(defun normalize (vector)
  "Normalizes the vector."
  (loop for x in vector
        with magnitude = (magnitude vector)
        collect (/ x magnitude)))

(defun +-v (&rest vectors)
  "Adds VECTORS together."
  (apply #'mapcar #'+ vectors))

(defun --v (&rest vectors)
  "Subtracts the rest of the VECTORS from the first."
  (apply #'mapcar #'- vectors))

(defun *-v (&rest factors)
  "Generalizes multiplication for scalars and vectors. Multiplies 2 vectors component-wise."
  (let ((product 1))
    (dolist (factor factors product)
      (setf product (if (atom product)
                        (if (atom factor)
                            (* product factor)
                            (scale-v factor product))
                        (if (atom factor)
                            (scale-v product factor)
                            (hadamard-* product factor)))))))

(defun hadamard-* (v1 v2)
  "Multiples V1 and V2 component-wise. The Hadamard product."
  (mapcar #'* v1 v2))

(defun scale-v (vector scalar)
  "Scales VECTOR by SCALAR."
  (mapcar (lambda (x) (* x scalar)) vector))

;;;lighting
;;define as dynamic variables for now
;;closure later
;;everything are lists for now
(defparameter ambient '(50 50 50))
(defparameter view '(0 0 1))
(defparameter light '(-.5 1 1))
(defparameter light-color '(255 0 255))
(defparameter areflect '(0.2 0.2 0.2))
(defparameter dreflect '(0.7 0.7 0.7))
(defparameter sreflect '(0.8 0.8 0.8))

(defun bound (color)
  "Checks and fixes the values of color."
  (mapcar (lambda (x) (min 255 (max 0 x))) color))

(defun rbound (color)
  "Rounds and checks bounds of color."
  (bound (mapcar #'round color)))

(defun calculate-color (normal)
  "Calculates the color given the NORMAL surface."
  (let ((normal (normalize normal))
        (light (normalize light)))
    (rbound
     (+-v (bound (*-v ambient areflect))
          (bound (*-v light-color dreflect (dot normal light)))
          (bound (*-v light-color sreflect
                      (expt (max 0 (dot view (--v (*-v 2 (dot normal light) normal)
                                                  light)))
                            16)))))))
