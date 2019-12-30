(use-package :raytracer)

(defrender draw2 #P"~/dev/lisp/src/raytracer/sphere2.png"
  (view-transform
   (point 1 3 -14)
   (point 0 3.5 0)
   (vectorr 0 1 0))
  (make-light
   :position (point -10 10 -10)
   :intensity (color 0.5 0.5 0.5))
  ((make-plane +identity-matrix+ checkers)
   (make-plane (m* (translation 0 0 30)
                   (rotation-y (/ pi 4))
                   (rotation-x (/ pi 2)))
               checkers)
   (make-plane (m* (translation 0 0 30)
                   (rotation-y (* 3 (/ pi 4)))
                   (rotation-x (/ pi 2)))
               checkers)
   (make-sphere
    (m*
     (translation -5 1 10)
     (scaling 1 1 1))
    (make-material
     :colour +red+
     :shininess 300
     :reflective 0.0))
   (make-sphere
    (m*
     (translation 1.5 2 15)
     (scaling 2 2 2))
    (make-material
     :colour +blue+
     :reflective 0.0))
   (make-sphere
    (m*
     (translation 0 3.5 0)
     (scaling 3 3 3))
    (make-material
     :transparency 1.0
     :reflective 0.1
     :refractive-index 2.9)))
  (checkers (make-material
             :colour (color 1 0.1 0.1)
             :specular 0
             :reflective 0
             :pattern (make-checkers-pattern +black+ +white+))))

(defrender draw1 #P"~/dev/lisp/src/raytracer/sphere1.png"
  (view-transform
   (point 0 3 -10)
   (point 0 1 0)
   (vectorr 0 1 0))
  (make-light
   :position (point -1 15 -20)
   :intensity (color 1 1 1))
  ((make-plane +identity-matrix+ checkers)
   (make-plane
    (m* (translation 0 0 10)
        (rotation-y (/ pi 4))
        (rotation-x (/ pi 2)))
    checkers)
   (make-plane
    (m* (translation 0 0 10)
        (rotation-y (- (/ pi 4)))
        (rotation-x (/ pi 2)))
    checkers)
   (make-sphere
    (m* (translation 1 2 5)
        (scaling 0.6 0.6 0.6))
    (make-material
     :colour +blue+
     :diffuse 1
     :reflective 0
     :specular 0.9))
   (make-sphere
    (translation -0.5 2 4)
    (make-material
     :colour +orange+
     :diffuse 0.9
     :reflective 0.5
     :transparency 0.5
     :specular 0.1))
   (glass-sphere (translation 0.5 2 2)))
  (checkers (make-material
             :colour (color 1 0.1 0.1)
             :specular 0
             :reflective 0.1
             :pattern (make-checkers-pattern +black+ +white+))))

(defrender draw0 #P"~/dev/lisp/src/raytracer/sphere0.png"
  (view-transform (point 0 1.5 -5)
                  (point 0 1 0)
                  (vectorr 0 1 0))
  (make-light
   :position (point -10 10 -10)
   :intensity (color 1 1 1))
  ((make-plane
    +identity-matrix+
    (make-material
     :colour (color 1 0.1 0.1)
     :specular 0
     :reflective 0.1
     :pattern (make-stripe-pattern +red+ +green+)))
   (make-plane
    (m* (translation 0 0 15) (rotation-x (/ pi 2) ))
    (make-material
     :colour (color 0.1 0.1 1)
     :pattern (make-checkers-pattern +blue+ +yellow+)
     :specular 0))
   (make-sphere
    (translation -0.5 1 0.5)
    (make-material
     :colour (color 0.1 1 0.5)
     :diffuse 0.7
     :reflective 0.5
     :pattern (make-gradient-pattern +red+ +orange+ (rotation-y (/ pi 2)))
     :specular 0.3))
   (make-sphere
    (m* (translation 1.5 1 -0.5)
        (scaling 0.5 0.5 0.5))
    (make-material
     :colour (color 0.5 1 0.1)
     :diffuse 0.7
     :reflective 0.9
     :pattern (make-ring-pattern +red+ +blue+ (scaling 0.1 0.1 0.1))
     :specular 0.3))
   (make-sphere
    (m* (translation -1.5 0.33 -0.75)
        (scaling 0.33 0.33 0.33))
    (make-material
     :colour (color 1 0.8 0.1)
     :diffuse 0.7
     :pattern (make-stripe-pattern +lime+ +green+)
     :specular 0.3))))
