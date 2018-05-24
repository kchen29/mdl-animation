;;;; Interacting with screens. Primarily interfacing to the outside.

(defconstant +screen-side+ 500)
(defparameter *screen* (make-array '(500 500) :initial-element '(0 0 0)))
(defparameter *z-buffer* (make-array '(500 500) :initial-element most-negative-double-float))
(declaim (type (simple-array list (500 500)) *screen*)
         (type (simple-array number (500 500)) *z-buffer*))

(defun plot (x y z color)
  "Plots (x, y) on *SCREEN* with COLOR. Checks bounds.
   COLOR is not copied. Checks the z-value with *z-buffer*."
  ;;round to 3 decimal places
  (setf z (fround-to z 3))
  (when (and (< -1 x +screen-side+) (< -1 y +screen-side+)
             (> z (aref *z-buffer* x y)))
    (setf (aref *screen* x y) color
          (aref *z-buffer* x y) z)))

(defun write-ppm (filename)
  "Writes a ppm, assuming P6 and max color value of 255.
   Writes to FILENAME with *SCREEN*."
  (with-open-file (stream filename :direction :output :if-exists :supersede :element-type :default)
    (print-screen stream)))

(defun print-screen (stream)
  "Prints *SCREEN* to STREAM. Returns STREAM."
  ;;fairly optimized
  (declare (optimize (speed 3) (debug 0))
           (type stream stream))
  (format stream "P6 ~a ~a 255~%" +screen-side+ +screen-side+)
  (do ((y (1- +screen-side+) (1- y)))
      ((< y 0))
    (dotimes (x +screen-side+)
      (dolist (c (aref *screen* x y))
        (write-byte c stream))))
  stream)

(defun save (filename)
  "Saves *SCREEN* to filename.
   Attempts conversion using imagemagick's convert if filename is not a ppm."
  (if (equal (pathname-type (pathname filename)) "ppm")
      (write-ppm filename)
      (close (print-screen (process-input (run-program "convert" (list "-" filename)
                                                       :input :stream
                                                       :wait nil :search t))))))

(defun clear-screen ()
  "Clears *SCREEN*. Sets all the pixels to black.
   Clears *Z-BUFFER*. Sets all the values to the least float."
  (dotimes (x +screen-side+)
    (dotimes (y +screen-side+)
      (setf (aref *screen* x y) '(0 0 0)
            (aref *z-buffer* x y) most-negative-double-float))))

(defun display ()
  "Displays the image with *SCREEN*.
   If WAIT is t, then will wait until display ends
   Uses imagemagick's display to display an image."
  (close (print-screen (process-input (run-program "display" (list "-")
                                                   :input :stream
                                                   :wait nil :search t)))))

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
