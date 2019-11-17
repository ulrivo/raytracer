(in-package :raytracer)

(defstruct ray
  (origin (point 0 0 0))
  (direction (vectorr 0 0 1)))

(defstruct intersektion
  (tt 0)
  (object (default-sphere)))

(defstruct light
  (position (point 0 0 0))
  (intensity (color 1 1 1)))

(defstruct material
  (colour (color 1 1 1))
  (ambient 0.1)
  (diffuse 0.9)
  (specular 0.9)
  (shininess 200))

(defclass shape ()
  ((transform :accessor shape-transform
              :initarg :transform
              :initform *identity-matrix*)
   (material :accessor shape-material
             :initarg :material
             :initform (make-material))))

(defclass sphere (shape) ())

(defun default-sphere ()
  (make-instance 'sphere))

(defun make-sphere (transform material)
  (make-instance 'sphere
                 :transform transform
                 :material material))

(defstruct world light shapes)

(defun default-world ()
  (make-world
   :light (make-light
           :position (point -10 10 -10)
           :intensity (color 1 1 1))
   :shapes (list
            (make-sphere
             *identity-matrix*
             (make-material
              :colour (color 0.8 1.0 0.6)
              :diffuse 0.7
              :specular 0.2))
            (make-sphere
             (scaling 0.5 0.5 0.5)
             (make-material)))))

(defstruct computations
  tt object point over-point eyev normalv inside )
