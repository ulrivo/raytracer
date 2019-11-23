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

(defclass pattern ()
  ((transform :accessor pattern-transform
              :initarg :transform
              :initform +identity-matrix+)
   (color1 :accessor pattern-color1
           :initarg :color1
           :initform +black+)
   (color2 :accessor pattern-color2
           :initarg :color2
           :initform +white+)))

(defclass stripe-pattern (pattern) ())
(defclass gradient-pattern (pattern) ())
(defclass ring-pattern (pattern) ())
(defclass checkers-pattern (pattern) ())

(defun make-stripe-pattern (color1 color2 transform)
  (make-instance 'stripe-pattern
                 :color1 color1
                 :color2 color2
                 :transform transform))

(defun make-gradient-pattern (color1 color2 transform)
  (make-instance 'gradient-pattern
                 :color1 color1
                 :color2 color2
                 :transform transform))

(defun make-ring-pattern (color1 color2 transform)
  (make-instance 'ring-pattern
                 :color1 color1
                 :color2 color2
                 :transform transform))

(defun make-checkers-pattern (color1 color2 transform)
  (make-instance 'checkers-pattern
    :color1 color1
    :color2 color2
    :transform transform))

(defstruct material
  (colour (color 1 1 1))
  (ambient 0.1)
  (diffuse 0.9)
  (specular 0.9)
  (shininess 200)
  (pattern nil))

(defclass shape ()
  ((transform :accessor shape-transform
              :initarg :transform
              :initform +identity-matrix+)
   (material :accessor shape-material
             :initarg :material
             :initform (make-material))))

(defclass sphere (shape) ())

(defun default-sphere () (make-instance 'sphere))

(defun make-sphere (transform material)
  (make-instance 'sphere
                 :transform transform
                 :material material))

(defclass plane (shape) ())

(defun default-plane () (make-instance 'plane))

(defun make-plane (transform material)
  (make-instance 'plane
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
             +identity-matrix+
             (make-material
              :colour (color 0.8 1.0 0.6)
              :diffuse 0.7
              :specular 0.2))
            (make-sphere
             (scaling 0.5 0.5 0.5)
             (make-material)))))

(defstruct computations
  tt object point over-point eyev normalv inside )

(defstruct camera
  hsize vsize field-of-view transform
  pixel-size half-width half-height)

(defun create-camera (hsize vsize field-of-view
                      &optional (transform +identity-matrix+))
  (let* ((half-view (tan (/ field-of-view 2)))
         (aspect (/ hsize vsize))
         (half-width half-view)
         (half-height half-view)
         (pixel-size))
    (if (<= 1 aspect)
        (setf half-height (/ half-view aspect))
        (setf half-width  (* half-view aspect)))
    (setf pixel-size (/ (* half-width 2) hsize))
    (make-camera
     :hsize hsize
     :vsize vsize
     :field-of-view field-of-view
     :transform transform
     :pixel-size pixel-size
     :half-width half-width
     :half-height half-height)))
