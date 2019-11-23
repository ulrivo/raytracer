(in-package :raytracer)

(defun ray-position (ray s)
  (tadd (ray-origin ray) (mults (ray-direction ray) s)))

(defun transform (ray matrix)
  (make-ray
   :origin (mm matrix (ray-origin ray))
   :direction (mm matrix (ray-direction ray))))

;; answers an intersektion, for all shapes first convert ray into object space
(defun intersect (shape ray)
  (let ((local-ray (transform ray (inverse (shape-transform shape)))))
    (local-intersect shape local-ray)))

(defmethod local-intersect ((s sphere) ray)
  (let* ((sphere-to-ray (tsub (ray-origin ray) (point 0 0 0)))
         (a (dot (ray-direction ray) (ray-direction ray)))
         (b (* 2 (dot (ray-direction ray) sphere-to-ray)))
         (c (1- (dot sphere-to-ray sphere-to-ray)))
         (discriminant (- (* b b) (* 4 a c))))
    (if (> 0 discriminant)
        '()
        (let ((t1 (/ (- (- b) (sqrt discriminant)) (* 2 a)))
              (t2 (/ (+ (- b) (sqrt discriminant)) (* 2 a))))
          (list
           (make-intersektion :tt t1 :object s)
           (make-intersektion :tt t2 :object s))))))

(defmethod local-intersect ((p plane) ray)
  (let ((y (aref (ray-direction ray) 1)))
        (if (< (abs y) +epsilon+)
            nil
            (let ((tt (- (/ (aref (ray-origin ray) 1) y))))
              (list (make-intersektion :tt tt :object p))))))

;; answer the intersection with the minimum of the non-negative tt-values
(defun hit (intersektions)
  (let ((mini nil))
    (dolist (i intersektions)
      (when (and (< 0 (intersektion-tt i))
                 (or (null mini)
                     (< (intersektion-tt i) (intersektion-tt mini))))
        (setf mini i)))
    mini))

(defun normal-at (shape world-point) 
  "wrapper for local-normal-at for all shapes"
  (let* ((inv (inverse (shape-transform shape)))
         (local-point (mm inv world-point))
         (local-normal (local-normal-at shape local-point))
         (world-normal (mm (transpose inv) local-normal)))
    (setf (aref world-normal 3) 0)
    (normalize world-normal)))

(defmethod local-normal-at ((s sphere) local-point)
  "the normal vector on a sphere at local-point"
  (tsub local-point (point 0 0 0)))

(defmethod local-normal-at ((p plane) local-point)
  "the normal vector on a plane is constant"
  (vectorr 0 1 0))

(defun reflect (in normal)
  (tsub in (mults normal (* 2 (dot in normal))))) 

(defmethod pattern-at ((pattern stripe-pattern) point)
    (if (evenp (floor (aref point 0)))
        (pattern-color1 pattern)
        (pattern-color2 pattern)))

(defmethod pattern-at ((pattern gradient-pattern) point)
  (let ((distance (tsub (pattern-color2 pattern) (pattern-color1 pattern)))
        (fraction (- (aref point 0) (floor (aref point 0)))))
    (tadd (pattern-color1 pattern) (mults distance fraction))))

(defmethod pattern-at ((pattern ring-pattern) point)
  (if (zerop (mod
              (floor (sqrt
                      (+ (* (aref point 0) (aref point 0))
                         (* (aref point 2) (aref point 2))))) 2))
      (pattern-color1 pattern)
      (pattern-color2 pattern)))

(defmethod pattern-at ((pattern checkers-pattern) point)
  (if (zerop (mod (+ (floor (aref point 0))
                     (floor (aref point 1))
                     (floor (aref point 2))) 2))
      (pattern-color1 pattern)
      (pattern-color2 pattern)))

(defun pattern-at-shape (pattern shape world-point)
  (let* ((object-point (mm (inverse (shape-transform shape)) world-point))
         (pattern-point (mm (inverse (pattern-transform pattern)) object-point)))
    (pattern-at pattern pattern-point)))

(defun lighting (material shape light point eyev normalv in-shadow)
  (let* ((color (if (material-pattern material)
                    (pattern-at-shape (material-pattern material) shape point)
                    (material-colour material)))
         (effective-color (hadamard-product color (light-intensity light)))
         (lightv (normalize (tsub (light-position light) point)))
         (ambient (mults effective-color (material-ambient material))))
    (if in-shadow
        ambient
        (progn
          (let* ((diffuse (color 0 0 0))
                 (specular (color 0 0 0))
                 (light-dot-normal (dot lightv normalv)))
            (when (<=  0 light-dot-normal)
              (setf diffuse (mults effective-color
                                   (* (material-diffuse material)
                                      light-dot-normal)))
              (let* ((reflectv (reflect (mults lightv -1) normalv))
                     (reflect-dot-eye (dot reflectv eyev)))
                (when (> reflect-dot-eye 0)
                  (let ((factor (expt reflect-dot-eye (material-shininess material))))
                    (setf specular (mults
                                    (light-intensity light)
                                    (* (material-specular material)
                                       factor)))))))
            (tadd ambient (tadd diffuse specular)))))))

(defun intersect-world (world ray)
  (sort
        (mapcan
         (lambda (s) (intersect s ray))
         (world-shapes world))
        #'< :key #'intersektion-tt))

(defun prepare-computations (intersektion ray)
  (let* ((tt (intersektion-tt intersektion))
         (object (intersektion-object intersektion))
         (point (ray-position ray tt))
         (eyev (mults (ray-direction ray) -1))
         (normalv (normal-at object point))
         (inside (< (dot normalv eyev) 0)))
    (when inside (setf normalv (mults normalv -1)))
    (make-computations
     :tt tt
     :object object
     :point point
     :over-point (tadd point (mults normalv +epsilon+))
     :eyev eyev
     :normalv normalv
     :inside inside)))

(defun is-shadowed (world point)
  (let* ((v (tsub (light-position (world-light world)) point))
         (distance (magnitude v))
         (direction (normalize v))
         (r (make-ray :origin point :direction direction))
         (is (intersect-world world r))
         (h (hit is)))
    (and h (< (intersektion-tt h) distance))))

(defun shade-hit (world comps)
  (let ((shadowed (is-shadowed world (computations-over-point comps))))
    (lighting (shape-material (computations-object comps))
              (computations-object comps)
              (world-light world)
              (computations-point comps)
              (computations-eyev comps)
              (computations-normalv comps)
              shadowed)))

(defun color-at (world ray)
  (let ((h (hit (intersect-world world ray))))
    (if h
        (shade-hit world (prepare-computations h ray))
        (color 0 0 0))))
