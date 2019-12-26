(use-package :raytracer)

(defun draw2 (width height)
  (let* ((checkers (make-material
                    :colour (color 1 0.1 0.1)
                    :specular 0
                    :reflective 0.05
                    :pattern (make-checkers-pattern +black+ +white+)))
         (floor (make-plane +identity-matrix+ checkers))
         (left-wall (make-plane (m* (translation 0 0 30)
                                    (rotation-y (/ pi 4))
                                    (rotation-x (/ pi 2)))
                                checkers))
         (right-wall (make-plane (m* (translation 0 0 30)
                                     (rotation-y (* 3 (/ pi 4)))
                                    (rotation-x (/ pi 2)))
                                checkers))
         (red-ball (make-sphere
                     (m* 
                      (translation -5 1 20)
                      (scaling 1 1 1))
                     (make-material
                      :colour +red+
                      :shininess 300
                      :reflective 0.0)))
         (blue-ball (make-sphere
                     (m* 
                      (translation 1.5 2 20)
                      (scaling 2 2 2))
                     (make-material
                      :colour +blue+
                      :reflective 0.0)))
         (glass-ball (make-sphere
                (m*
                 (translation 0 3.5 0)
                 (scaling 3 3 3))
                (make-material
                 :transparency 1.0
                 :refractive-index 3)))
         (camera (create-camera  width height (/ pi 3)
                                 (view-transform
                                          (point 1 3 -14)
                                          (point 0 3.5 0)
                                          (vectorr 0 1 0))))
         (world (make-world
                 :light (make-light
                         :position (point -10 10 -10)
                         :intensity (color 0.5 0.5 0.5))
                 :shapes (list
                          floor
                          left-wall
                          right-wall
                          blue-ball
                          red-ball
                          glass-ball)))
         (canvas (render camera world)))
    (save-canvas canvas "~/dev/lisp/src/raytracer/sphere2.png")))

(defun draw1 (width height)
  (let* ((checkers (make-material
                    :colour (color 1 0.1 0.1)
                    :specular 0
                    :reflective 0.1
                    :pattern (make-checkers-pattern +black+ +white+)))
         (floor (make-plane +identity-matrix+ checkers))
         (right (make-plane
                 (m* (translation 0 0 10)
                     (rotation-y (/ pi 4))
                     (rotation-x (/ pi 2)))
                 checkers))
         (left (make-plane
                (m* (translation 0 0 10)
                    (rotation-y (- (/ pi 4)))
                     (rotation-x (/ pi 2)))
                 checkers))
         (right-s (make-sphere
                   (m* (translation 1 2 5)
                       (scaling 0.6 0.6 0.6))
                  (make-material
                   :colour +blue+
                   :diffuse 1
                   :reflective 0
                   :specular 0.9)))
         (middle (make-sphere
                  (translation -0.5 2 4)
                  (make-material
                   :colour +orange+
                   :diffuse 0.9
                   :reflective 0.5
                   :transparency 0.5
                   :specular 0.1)))
         (front (glass-sphere (translation 0.5 2 2)))
         (camera (create-camera  width height (/ pi 3)
                                 (view-transform (point 0 3 -10)
                                          (point 0 1 0)
                                          (vectorr 0 1 0))))
         (world (make-world
                 :light (make-light
                         :position (point -1 15 -20)
                         :intensity (color 1 1 1))
                 :shapes (list floor right left right-s middle front)))
         (canvas (render camera world)))
    (save-canvas canvas "sphere1.png")))

(defun draw (width height)
  (let* ((floor (make-plane
                 +identity-matrix+
                 (make-material
                  :colour (color 1 0.1 0.1)
                  :specular 0
                  :reflective 0.1
                  :pattern (make-stripe-pattern +red+ +green+))))
         (back (make-plane
                (m* (translation 0 0 15) (rotation-x (/ pi 2) ))
                 (make-material
                  :colour (color 0.1 0.1 1)
                  :pattern (make-checkers-pattern +blue+ +yellow+)
                  :specular 0)))
         (middle (make-sphere
                  (translation -0.5 1 0.5)
                  (make-material
                   :colour (color 0.1 1 0.5)
                   :diffuse 0.7
                   :reflective 0.5
                   :pattern (make-gradient-pattern +red+ +orange+ (rotation-y (/ pi 2)))
                   :specular 0.3)))
         (right (make-sphere
                 (m* (translation 1.5 1 -0.5)
                     (scaling 0.5 0.5 0.5))
                 (make-material
                  :colour (color 0.5 1 0.1)
                  :diffuse 0.7
                  :reflective 0.9
                  :pattern (make-ring-pattern +red+ +blue+ (scaling 0.1 0.1 0.1))
                  :specular 0.3)))
         (left (make-sphere
                (m* (translation -1.5 0.33 -0.75)
                    (scaling 0.33 0.33 0.33))
                (make-material
                 :colour (color 1 0.8 0.1)
                 :diffuse 0.7
                 :pattern (make-stripe-pattern +lime+ +green+)
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
