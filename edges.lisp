;;;; Add things to edges.

(defun add-edge (edges x0 y0 z0 x1 y1 z1)
  "Adds a line from point (x0 y0 z0) to (x1 y1 z1) to EDGES.
   Adjusts the array."
  (add-point edges x0 y0 z0)
  (add-point edges x1 y1 z1))

(defun add-point (edges x y z)
  "Adds a point (x y z) to EDGES."
  (with-args (cols last-col) (matrix edges)
    (when (= last-col cols)
      (adjust-matrix edges 4 (* 2 cols)))
    (setf (mref edges 0 last-col) x
          (mref edges 1 last-col) y
          (mref edges 2 last-col) z
          (mref edges 3 last-col) 1)
    (incf last-col)))

;;;curves
(defun add-parametric (edges step x-function y-function &optional (z 0))
  "Given X-FUNCTION and Y-FUNCTION, which take one input and outputs the x and y
   coordinates respectively, add a parametric where s runs from 0 to 1 at STEP interval
   and add the connecting lines to EDGES.
   Optionally takes a z value, defaulted to 0, where all the points are shifted by z."
  (flet ((get-x (s) (funcall x-function s))
         (get-y (s) (funcall y-function s)))
    (do* ((s step (+ s step))
          (prev-x (get-x 0) x)
          (prev-y (get-y 0) y)
          (x (get-x s) (get-x s))
          (y (get-y s) (get-y s)))
         ((>= s (1+ step)))
      (add-edge edges prev-x prev-y z x y z))))

(defun add-circle (edges step x y z radius)
  "Add a circle to EDGES with center (x y) and RADIUS with STEP interval. Circle shifted by Z."
  (add-parametric edges step
                  (lambda (s) (+ x (* radius (cos (* 2 pi s)))))
                  (lambda (s) (+ y (* radius (sin (* 2 pi s)))))
                  z))

(defun add-hermite (edges step x0 y0 x1 y1 dx0 dy0 dx1 dy1)
  "Add a hermite curve to EDGES with points (x0 y0) and (x1 y1) and the rates wrt. time of
   the corresponding coordinates (dx0 dy0) and (dx1 dy1), with STEP interval."
  (add-parametric edges step
                  (get-hermite-cubic x0 x1 dx0 dx1)
                  (get-hermite-cubic y0 y1 dy0 dy1)))

(defun get-hermite-cubic (x0 x1 dx0 dx1)
  "Returns the function, given the coordinate (x0 x1) and rates of changes (dx0 dx1),
   taking in a time and returning the output on a hermite cubic curve."
  (lambda (s) (evaluate-polynomial s
                                   x0 dx0
                                   (- (* 3 x1) (* 3 x0) (* 2 dx0) dx1)
                                   (+ (* 2 x0) (* -2 x1) dx0 dx1))))

(defun add-bezier (edges step x0 y0 x1 y1 x2 y2 x3 y3)
  "Add a bezier curve to EDGES with endpoints (x0 y0) and (x3 y3).
   (x1 y1) and (x2 y2) are control points. Drawn with STEP interval."
  (add-parametric edges step
                  (get-bezier-cubic x0 x1 x2 x3)
                  (get-bezier-cubic y0 y1 y2 y3)))

(defun get-bezier-cubic (x0 x1 x2 x3)
  "Returns the function, given the x coordinates, taking in a time and returning the output
   on a bezier cubic curve."
  (lambda (s) (evaluate-polynomial s
                                   x0 (* 3 (- x1 x0))
                                   (- (* 3 (+ x0 x2)) (* 6 x1))
                                   (+ (* 3 (- x1 x2)) (- x3 x0)))))

;;;3d shapes
(defun add-polygon (polygons x0 y0 z0 x1 y1 z1 x2 y2 z2)
  "Adds a triangle defined by the given points to POLYGONS."
  (add-point polygons x0 y0 z0)
  (add-point polygons x1 y1 z1)
  (add-point polygons x2 y2 z2))

(defun add-quad (polygons x0 y0 z0 x1 y1 z1 x2 y2 z2 x3 y3 z3)
  "Adds a quadrilateral to POLYGONS. Connects first three points
   as a triangle, and first, third, and fourth points as a triangle."
  (add-polygon polygons x0 y0 z0 x1 y1 z1 x2 y2 z2)
  (add-polygon polygons x0 y0 z0 x2 y2 z2 x3 y3 z3))

(defun add-quad-index (polygons points i j k l)
  "Adds a quadrilateral to POLYGONS, with indices.
   Indices into points."
  (macrolet-helper
    `(add-quad polygons
               ,@(generate ((x '(i j k l)) (y 3))
                   `(mref points ,y ,x)))))

(defun add-box (polygons x y z width height depth)
  "Adds a box to POLYGONS where the front left upper point is (x y z).
   WIDTH is x, HEIGHT y, and DEPTH z."
  (let ((r (+ x width))
        (d (- y height))
        (b (- z depth)))
    ;;front
    (add-quad polygons x y z x d z r d z r y z)
    ;;top
    (add-quad polygons x y z r y z r y b x y b)
    ;;left
    (add-quad polygons x y z x y b x d b x d z)
    ;;back
    (add-quad polygons r d b x d b x y b r y b)
    ;;bottom
    (add-quad polygons r d b r d z x d z x d b)
    ;;right
    (add-quad polygons r d b r y b r y z r d z)))

(defun generate-sphere (step x y z r)
  "Generates a sphere with center (x y z), radius R, points drawn STEP times."
  (let ((points (make-matrix)))
    (do-step-max (phi step (* 2 pi))
      (do-step-max (theta step pi)
        (add-point points (+ x (* r (cos theta)))
                   (+ y (* r (sin theta) (cos phi)))
                   (+ z (* r (sin theta) (sin phi))))))
    points))

(defun get-index (step rotation circle)
  "Give the index of the point given STEP, ROTATION, and CIRCLE."
  (+ circle (* rotation (1+ step))))

(defun add-sphere (polygons step x y z r)
  "Adds a sphere to POLYGONS."
  (let ((points (generate-sphere step x y z r)))
    (dotimes (rot step)
      (dotimes (cir (1- step))
        (add-quad-index polygons points
                        (get-index step rot (1+ cir))
                        (get-index step (1+ rot) (+ 2 cir))
                        (get-index step (1+ rot) (1+ cir))
                        (get-index step rot cir))))))

(defun generate-torus (step x y z r1 r2)
  "Generates a torus with center (x y z), cross-section circle radius R1,
   rotated around with radius R2. Points drawn STEP times."
  (let ((points (make-matrix)))
    (do-step-max (phi step (* 2 pi))
      (do-step-max (theta step (* 2 pi))
        (add-point points (+ x (* (cos phi) (+ r2 (* r1 (cos theta)))))
                   (+ y (* r1 (sin theta)))
                   (- z (* (sin phi) (+ r2 (* r1 (cos theta))))))))
    points))

(defun add-torus (polygons step x y z r1 r2)
  "Adds a torus to POLYGONS."
  (let ((points (generate-torus step x y z r1 r2)))
    (dotimes (rot step)
      (dotimes (cir step)
        (add-quad-index polygons points
                        (get-index step rot cir)
                        (get-index step (1+ rot) cir)
                        (get-index step (1+ rot) (1+ cir))
                        (get-index step rot (1+ cir)))))))
