(use-package :raytracer)

(defun draw (width height)
  (let* ((floor (make-plane
                 +identity-matrix+
                 (make-material
                  :colour (color 1 0.1 0.1)
                  :specular 0
                  :pattern (make-stripe-pattern +lime+ +green+ +identity-matrix+))))
         (back (make-plane
                (mm (translation 0 0 5) (rotation-x (/ pi 2) ))
                 (make-material
                  :colour (color 0.1 0.1 1)
                  :pattern (make-checkers-pattern +blue+ +green+ +identity-matrix+)
                  :specular 0)))
         (middle (make-sphere
                  (translation -0.5 1 0.5)
                  (make-material
                   :colour (color 0.1 1 0.5)
                   :diffuse 0.7
                   :pattern (make-gradient-pattern +green+ +orange+ (rotation-y (/ pi 2)))
                   :specular 0.3)))
         (right (make-sphere
                 (mm (translation 1.5 1 -0.5)
                     (scaling 0.5 0.5 0.5))
                 (make-material
                  :colour (color 0.5 1 0.1)
                  :diffuse 0.7
                  :pattern (make-ring-pattern +red+ +blue+ (scaling 0.1 0.1 0.1))
                  :specular 0.3)))
         (left (make-sphere
                (mm (translation -1.5 0.33 -0.75)
                    (scaling 0.33 0.33 0.33))
                (make-material
                 :colour (color 1 0.8 0.1)
                 :diffuse 0.7
                 :pattern (make-stripe-pattern +lime+ +green+ +identity-matrix+)
                 :specular 0.3)))
         (camera (create-camera  width height (/ pi 3)
                                 (view-transform (point 0 1.5 -5)
                                                 (point 0 1 0)
                                                 (vectorr 0 1 0))))
         (world (make-world
                 :light (make-light
                         :position (point -10 10 -10)
                         :intensity (color 1 1 1))
                 :shapes (list floor back middle right left )))
         (canvas (render camera world)))
    (save-canvas canvas "sphere500.png")))
