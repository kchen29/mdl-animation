;;;; Draw to screen.

(defun draw-line (x0 y0 z0 x1 y1 z1 color)
  "Draws a line from (x0, y0) to (x1, y1) on *SCREEN* using COLOR."
  (roundify x0 y0 x1 y1)
  (sortify 0 (x0 y0 z0) (x1 y1 z1))
  (let* ((xdif (- x1 x0))
         (ydif (- y1 y0))
         (zdif (- z1 z0))
         (aydif (abs ydif)))
    (cond
      ((and (zerop xdif) (zerop ydif)) (plot x0 y0 (max z0 z1) color))
      ((< aydif xdif)
       (do ((x x0 (1+ x))
            (y-s (float (/ ydif xdif)))
            (y (float y0) (+ y y-s))
            (z-s (float (/ zdif xdif)))
            (z (float z0) (+ z z-s)))
           ((> x x1))
         (plot x (round y) z color)))
      (t (do ((x-s (float (/ xdif aydif)))
              (x (float x0) (+ x x-s))
              (y-s (signum ydif))
              (y y0 (+ y y-s))
              (z-s (float (/ zdif aydif)))
              (z (float z0) (+ z z-s)))
             ((> y y1))
           (plot (round x) y z color))))))

(defun draw-line-index (edges index color)
  "Draws the line starting from INDEX in EDGES."
  (macrolet-helper
    `(draw-line
      ,@(generate ((in 2) (co 3))
          `(mref edges ,co (+ ,in index)))
      color)))

(defun draw-lines (edges color)
  "Draws the lines from EDGES to *SCREEN* with COLOR."
  (do-step (index (m-last-col edges) 2)
    (draw-line-index edges index color)))

;;;3d shapes
(defun draw-polygon (x0 y0 z0 x1 y1 z1 x2 y2 z2 color)
  "Draws the polygon to *SCREEN*."
  (draw-line x0 y0 z0 x1 y1 z1 color)
  (draw-line x0 y0 z0 x2 y2 z2 color)
  (draw-line x1 y1 z1 x2 y2 z2 color)
  (scanline x0 y0 z0 x1 y1 z1 x2 y2 z2 color))

(defun draw-polygon-index (polygons index color)
  "Draws the polygon starting from INDEX in POLYGONS"
  (macrolet-helper
    `(draw-polygon
      ,@(generate ((in 3) (co 3))
          `(mref polygons ,co (+ ,in index)))
      color)))

(defun draw-polygons (polygons)
  "Draws the polygons from POLYGONS to *SCREEN*."
  (do-step (index (m-last-col polygons) 3)
    (let ((normal (normal polygons index)))
      (when (plusp (svref normal 2))
        (draw-polygon-index polygons index (calculate-color (coerce normal 'list)))))))

(defun scanline (x0 y0 z0 x1 y1 z1 x2 y2 z2 color)
  "Does scanline conversion."
  (roundify y0 y1 y2)
  ;;have y0 be the bottom, y1 the middle, and y2 the top
  (sortify 1 (x0 y0 z0) (x1 y1 z1) (x2 y2 z2))
  (let ((ydif1 (- y1 y0))
        (ydif2 (- y2 y0))
        (ydif3 (- y2 y1)))
    (do ((y y0 (1+ y))
         (a-s (safe-diff-quot x2 x0 ydif2))
         (a x0 (+ a a-s))
         (b-s1 (safe-diff-quot x1 x0 ydif1))
         (b-s2 (safe-diff-quot x2 x1 ydif3))
         (b x0)
         (c-s (safe-diff-quot z2 z0 ydif2))
         (c z0 (+ c c-s))
         (d-s1 (safe-diff-quot z1 z0 ydif1))
         (d-s2 (safe-diff-quot z2 z1 ydif3))
         (d z0))
        ((>= y y2))
      (cond
        ((< y0 y y1) (incf b b-s1) (incf d d-s1))
        ((< y1 y y2) (incf b b-s2) (incf d d-s2))
        ((= y y1) (setf b x1 d z1)))
      (draw-line a y c b y d color))))
