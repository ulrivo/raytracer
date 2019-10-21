(in-package :raytracer)

(defstruct ray
  (origin (point 0 0 0))
  (direction (vectorr 0 0 1)))

(defstruct sphere
  (transform *identity-matrix*))

(defstruct intersektion
  (tt 0)
  (object (make-sphere)))

(defun ray-position (ray s)
  (tadd (ray-origin ray) (mults (ray-direction ray) s)))

(defun transform (ray matrix)
  (make-ray
   :origin (mm matrix (ray-origin ray))
   :direction (mm matrix (ray-direction ray))))

;; answers an intersektion
(defun intersect (sphere ray)
  (let* ((ray2 (transform ray (inverse (sphere-transform sphere))))
         (sphere-to-ray (tsub (ray-origin ray2) (point 0 0 0)))
         (a (dot (ray-direction ray2) (ray-direction ray2)))
         (b (* 2 (dot (ray-direction ray2) sphere-to-ray)))
         (c (1- (dot sphere-to-ray sphere-to-ray)))
         (discriminant (- (* b b) (* 4 a c))))
    (if (> 0 discriminant)
        '()
        (let ((t1 (/ (- (- b) (sqrt discriminant)) (* 2 a)))
              (t2 (/ (+ (- b) (sqrt discriminant)) (* 2 a))))
          (list
           (make-intersektion :tt t1 :object sphere)
           (make-intersektion :tt t2 :object sphere))))))

;; answer the intersection with the minimum of the non-negative tt-values
(defun hit (intersektions)
  (let ((mini nil))
    (dolist (i intersektions)
      (when (and (< 0 (intersektion-tt i))
                 (or (null mini)
                     (< (intersektion-tt i) (intersektion-tt mini))))
        (setf mini i)))
    mini))

(defun normal-at (sphere world-point)
  (let* ((inv (inverse (sphere-transform sphere)))
         (object-point (mm inv world-point))
         (object-normal (tsub object-point (point 0 0 0)))
         (world-normal (mm (transpose inv) object-normal)))
    (setf (aref world-normal 3) 0)
    (normalize world-normal)))
