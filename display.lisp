;;;; Interacting with screens. Primarily interfacing to the outside.

(defparameter *screen-side* 0)
(defparameter *screen* 0)
(defparameter *z-buffer* 0)

(defun make-screen (side)
  (setf *screen-side* side)
  (let ((dimensions (list side side)))
    (setf *screen* (make-array dimensions :initial-element '(0 0 0)))
    (setf *z-buffer* (make-array dimensions :initial-element most-negative-double-float))))

(make-screen 500)

(defun plot (x y z color)
  "Plots (x, y) on *SCREEN* with COLOR. Checks bounds.
   COLOR is not copied. Checks the z-value with *z-buffer*."
  ;;round to 3 decimal places
  (setf z (fround-to z 3))
  (when (and (< -1 x *screen-side*) (< -1 y *screen-side*)
             (> z (aref *z-buffer* x y)))
    (setf (aref *screen* x y) color
          (aref *z-buffer* x y) z)))

(defun write-ppm (filename)
  "Writes a ppm, assuming P3 and max color value of 255.
   Writes to FILENAME with *SCREEN*."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (let ((*standard-output* stream))
      (print-screen))))

(defun print-screen ()
  "Prints *SCREEN* to standard out."
  (format t "P3 ~a ~a 255" *screen-side* *screen-side*)
  (loop for y from (1- *screen-side*) downto 0
        do (progn (format t "~%")
                  (loop for x below *screen-side*
                        do (format t "~{~a ~}" (aref *screen* x y))))))

(defun save (filename)
  "Saves *SCREEN* to filename.
   Attempts conversion using imagemagick's convert if filename is not a ppm."
  (if (equal (pathname-type (pathname filename)) "ppm")
      (write-ppm filename)
      (run-program "convert" (list "-" filename)
                   :input (make-string-input-stream (output-to-string (print-screen)))
                   :wait nil :search t)))

(defun clear-screen ()
  "Clears *SCREEN*. Sets all the pixels to black.
   Clears *Z-BUFFER*. Sets all the values to the least float."
  (dotimes (x *screen-side*)
    (dotimes (y *screen-side*)
      (setf (aref *screen* x y) '(0 0 0)
            (aref *z-buffer* x y) most-negative-double-float))))

(defun display (&optional (wait nil))
  "Displays the image with *SCREEN*.
   If WAIT is t, then will wait until display ends
   Uses imagemagick's display to display an image."
  (run-program "display" (list "-")
               :input (make-string-input-stream (output-to-string (print-screen)))
               :wait wait :search t))

(defun save-frame (basename frame digits)
  "Saves a frame with BASENAME, FRAME, and DIGITS for frames."
  (ensure-directories-exist "anim/")
  (save (format nil "anim/~a~a~a.png"
                basename
                (make-string (- digits (integer-digits frame)) :initial-element #\0)
                frame)))

(defun make-animation (basename)
  "Creates an animation using BASENAME."
  ;;make sure if the gif already exists, delete it
  (let ((gif (format nil "anim/~a.gif" basename)))
    (when (probe-file gif)
      (delete-file gif))
    (run-program "convert" (list "-delay"
                                 "3"
                                 (format nil "anim/~a*" basename)
                                 gif)
                 :wait nil :search t)))
