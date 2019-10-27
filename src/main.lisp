(in-package :raytracer)

(defun draw ()
  (let* ((ray-origin (point 0 0 -5))
         (wall-z 10)
         (wall-size 7)
         (canvas-pixel 500)
         (pixel-size (/ wall-size canvas-pixel))
         (half (/ wall-size 2))
         (canvas (canvas canvas-pixel canvas-pixel))
         (shape (make-sphere))
         (material (make-material :colour (color 1 0.2 1)))
         (light (make-light :position (point -10 10 -10) :intensity (color 1 1 1))))
    (dotimes (y canvas-pixel)
      (let ((world-y (- half (* pixel-size y))))
        (dotimes (x canvas-pixel)
          (let* ((world-x (+ (- half) (* pixel-size x)))
                 (position (point world-x world-y wall-z))
                 (r (make-ray
                     :origin ray-origin
                     :direction (normalize (tsub position ray-origin))))
                 (xs (intersect shape r)))
            (let ((h (hit xs)))
              (when h
                (let* ((point (ray-position r (intersektion-tt h)))
                       (normal (normal-at (intersektion-object h) point))
                       (eye (mults (ray-direction r) -1)))
                  (write-pixel canvas x y
                               (lighting material light point eye normal)))))))))
    (save-canvas canvas "sphere7.ppm")))

