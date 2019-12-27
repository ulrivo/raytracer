;; (in-package :raytracer)

(defmacro defrender (render-name file-name
                     camera-view-transform world-light
                     &body body)
  `(defun ,render-name (width height)
     (let* (,@ body
               (camera (create-camera width height
                          (/ pi 3) ,camera-view-transform))
               (world (make-world :light ,world-light))
               (canvas (render camera world)))
       (save-canvas canvas ,file-name))))

(defrender draw "draw.png"
    (view-transform (point 0 3 -10)
                    (point 0 1 0)
                    (vectorr 0 1 0))
    (make-light
     :position (point -1 15 -20)
     :intensity (color 1 1 1))
  (floor (make-plane +identity-matrix+ checkers)))