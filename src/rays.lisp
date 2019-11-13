(in-package :raytracer)

(defstruct ray
  (origin (point 0 0 0))
  (direction (vectorr 0 0 1)))

(defstruct sphere
  (transform *identity-matrix*)
  material)

(defstruct intersektion
  (tt 0)
  (object (make-sphere)))

(defstruct light
  (position (point 0 0 0))
  (intensity (color 1 1 1)))

(defstruct material
  (colour (color 1 1 1))
  (ambient 0.1)
  (diffuse 0.9)
  (specular 0.9)
  (shininess 200))

(defstruct world light shapes)

(defun default-world ()
  (make-world
  :light (make-light
          :position (point -10 10 -10)
          :intensity (color 1 1 1))
  :shapes (list
           (make-sphere
            :material (make-material
                       :colour (color 0.8 1.0 0.6)
                       :diffuse 0.7
                       :specular 0.2))
           (make-sphere
            :material (make-material)
            :transform (scaling 0.5 0.5 0.5)))))

(defstruct computations
  tt object point eyev normalv inside )

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

(defun reflect (in normal)
  (tsub in (mults normal (* 2 (dot in normal)))))

(defun lighting (material light point eyev normalv)
  (let* ((effective-color (hadamard-product (material-colour material)
                                            (light-intensity light)))
         (lightv (normalize (tsub (light-position light)
                                  point)))
         (ambient (mults effective-color (material-ambient material)))
         (diffuse (color 0 0 0))
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
                               factor)))
            ;; (format t "reflect-dot-eye ~A specular ~A~%" reflect-dot-eye specular)
            ))))
    (tadd ambient (tadd diffuse specular))))

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
         (inside (dot normalv eyev)))
    (when inside (setf normalv (mults normalv -1)))
    (make-computations
     :tt tt
     :object object
     :point point
     :eyev eyev
     :normalv normalv
     :inside inside)))

(defun all-function-symbols (package)
  "Retrieves all functions in a package."
  (let (symbols)
    (do-symbols (s package)
      (push s symbols))
    symbols))
