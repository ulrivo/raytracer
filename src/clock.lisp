(in-package :raytracer)

(defun draw (w h)
  (let* ((c (canvas w h))
         (center-x (truncate (/ w 2)))
         (center-y (truncate (/ h 2)))
         (to-center (translation center-x center-y 0))
         (radius (truncate (* 0.4 (min w h))))
         (to-12 (translation 0 radius 0))
         )
    (dotimes (i 12)
      (let* ((p (point 0 0 0))
             (rotate (rotation-z (* i (/ pi 6))))
             (transform-p (mm to-center (mm rotate (mm to-12 p)))))
        (write-pixel c
                     (aref transform-p 0)
                     (aref transform-p 1)
                     (color 1 0 0))))
    (save-canvas c "clock.ppm")))

(defun run-shot ()
  (draw 200 200))

