(in-package :raytracer)

(defun ray-position (ray s)
  (v+ (ray-origin ray) (v* (ray-direction ray) s)))

(defun transform (ray matrix)
  (make-ray
   :origin    (m* matrix (ray-origin ray))
   :direction (m* matrix (ray-direction ray))))

(defun view-transform (from to up)
  (let* ((forward (nvunit (v- to from)))
         (upn (nvunit up))
         (forward3 (vec3 (vx forward) (vy forward) (vz forward)))
         (upn3 (vec3 (vx upn) (vy upn) (vz upn)))
         (left (vc forward3 upn3))
         (true-up (vc left forward3))
         (nforward (v* forward3 -1))
         (nfrom (v* from -1))
         (orientation (mat
                       (vx left) (vy left) (vz left) 0
                       (vx true-up) (vy true-up) (vz true-up) 0
                       (vx nforward) (vy nforward) (vz nforward) 0
                       0 0 0 1)))
    (m* orientation (translation (vx nfrom) (vy nfrom) (vz nfrom)))))

;; answers an intersektion, for all shapes first convert ray into object space

(defun check-axis (origin direction)
  (let ((tmin-n (- -1 origin))
        (tmax-n (- 1 origin))
        (infinity float-features:single-float-positive-infinity)
        tmin tmax)
    (if (>= (abs direction) +epsilon+)
        (progn
          (setf tmin (/ tmin-n direction))
          (setf tmax (/ tmax-n direction)))
        (progn
          (setf tmin (* tmin-n infinity))
          (setf tmax (* tmax-n infinity))))
    (when (> tmin tmax)
      (rotatef tmin tmax))
    (values tmin tmax)))

(defmethod local-intersect ((s sphere) ray)
  (let* ((sphere-to-ray (v- (ray-origin ray) (point 0 0 0)))
         (a (v. (ray-direction ray) (ray-direction ray)))
         (b (* 2 (v. (ray-direction ray) sphere-to-ray)))
         (c (1- (v. sphere-to-ray sphere-to-ray)))
         (discriminant (- (* b b) (* 4 a c))))
    (if (> 0 discriminant)
        '()
        (let ((t1 (/ (- (- b) (sqrt discriminant)) (* 2 a)))
              (t2 (/ (+ (- b) (sqrt discriminant)) (* 2 a))))
          (list
           (make-intersektion :tt t1 :object s)
           (make-intersektion :tt t2 :object s))))))

(defmethod local-intersect ((p plane) ray)
  (let ((y (vy (ray-direction ray))))
        (if (< (abs y) +epsilon+)
            nil
            (let ((tt (- (/ (vy (ray-origin ray)) y))))
              (list (make-intersektion :tt tt :object p))))))

(defmethod local-intersect ((c cube) ray)
  (multiple-value-bind (xtmin xtmax)
      (check-axis (vx (ray-origin ray)) (vx (ray-direction ray)))
    (multiple-value-bind (ytmin ytmax)
        (check-axis (vy (ray-origin ray)) (vy (ray-direction ray)))
      (multiple-value-bind (ztmin ztmax)
          (check-axis (vz (ray-origin ray)) (vz (ray-direction ray)))
        (let
            ((tmin (max xtmin ytmin ztmin))
             (tmax (min xtmax ytmax ztmax)))
          (if (> tmin tmax)
              ()
              (list (make-intersektion :tt tmin :object c)
                    (make-intersektion :tt tmax :object c))))))))

(defun intersect (shape ray)
  (let ((local-ray (transform ray (minv (shape-transform shape)))))
    (local-intersect shape local-ray)))

;; answer the intersection with the minimum of the non-negative tt-values
(defun hit (intersektions)
  (let ((mini nil))
    (dolist (i intersektions)
      (when (and (< 0 (intersektion-tt i))
                 (or (null mini)
                     (< (intersektion-tt i) (intersektion-tt mini))))
        (setf mini i)))
    mini))

(defmethod local-normal-at ((s sphere) local-point)
  "the normal vector on a sphere at local-point"
  (v- local-point (point 0 0 0)))

(defmethod local-normal-at ((p plane) local-point)
  "the normal vector on a plane is constant"
  (vectorr 0 1 0))

(defun normal-at (shape world-point) 
  "wrapper for local-normal-at for all shapes"
  (let* ((inv (minv (shape-transform shape)))
         (local-point (m* inv world-point))
         (local-normal (local-normal-at shape local-point))
         (world-normal (m* (mtranspose inv) local-normal)))
    (setf (vw world-normal) 0)
    (vunit world-normal)))

(defun reflect (in normal)
  (v- in (v* normal (* 2 (v. in normal)))))

(defmethod pattern-at ((pattern test-pattern) point)
  (color (vx point)
         (vy point)
         (vz point)))

(defmethod pattern-at ((pattern stripe-pattern) point)
    (if (evenp (floor (vx point)))
        (pattern-color1 pattern)
        (pattern-color2 pattern)))

(defmethod pattern-at ((pattern gradient-pattern) point)
  (let ((distance (v- (pattern-color2 pattern) (pattern-color1 pattern)))
        (fraction (- (vx point) (floor (vx point)))))
    (v+ (pattern-color1 pattern) (v* distance fraction))))

(defmethod pattern-at ((pattern ring-pattern) point)
  (if (zerop (mod
              (floor (sqrt
                      (+ (* (vx point) (vx point))
                         (* (vz point) (vz point))))) 2))
      (pattern-color1 pattern)
      (pattern-color2 pattern)))

(defmethod pattern-at ((pattern checkers-pattern) point)
  (if (zerop (mod (+ (floor (vx point))
                     (floor (vy point))
                     (floor (vz point))) 2))
      (pattern-color1 pattern)
      (pattern-color2 pattern)))

(defun pattern-at-shape (pattern shape world-point)
  (let* ((object-point (m* (minv (shape-transform shape)) world-point))
         (pattern-point (m* (minv (pattern-transform pattern)) object-point)))
    (pattern-at pattern pattern-point)))

(defun lighting (material shape light point eyev normalv in-shadow)
  (let* ((color (if (material-pattern material)
                    (pattern-at-shape (material-pattern material) shape point)
                    (material-colour material)))
         (effective-color (v* color (light-intensity light)))
         (lightv (vunit (v- (light-position light) point)))
         (ambient (v* effective-color (material-ambient material))))
    (if in-shadow
        ambient
        (let* ((diffuse (color 0 0 0))
               (specular (color 0 0 0))
               (light-dot-normal (v. lightv normalv)))
          (when (<=  0 light-dot-normal)
            (setf diffuse (v* effective-color
                              (* (material-diffuse material)
                                 light-dot-normal)))
            (let* ((reflectv (reflect (v* lightv -1) normalv))
                   (reflect-dot-eye (v. reflectv eyev)))
              (when (> reflect-dot-eye 0)
                (let ((factor (expt reflect-dot-eye (material-shininess material))))
                  (setf specular (v* (light-intensity light)
                                     (* (material-specular material) factor)))))))
          (v+ ambient diffuse specular)))))

(defun intersect-world (world ray)
  (sort
        (mapcan
         (lambda (s) (intersect s ray))
         (world-shapes world))
        #'< :key #'intersektion-tt))


(defun prepare-computations (intersektion ray &optional (inters (list intersektion)))
  (let* ((tt (intersektion-tt intersektion))
         (object (intersektion-object intersektion))
         (point (ray-position ray tt))
         (eyev (v* (ray-direction ray) -1))
         (normalv (normal-at object point))
         (inside (< (v. normalv eyev) 0))
         n1 n2 containers)
    (when inside (setf normalv (v* normalv -1)))
    ;; calculate n1 and n2 with the help of containers
    (dolist (i inters)
      (when (equalp i intersektion)
        (setf n1
              (if containers
                  (material-refractive-index
                   (shape-material
                    (first containers)))
                  1.0)))
      (setf containers (if (member (intersektion-object i) containers)
                           (remove (intersektion-object i) containers)
                           (cons (intersektion-object i) containers)))
      (when (equalp i intersektion)
        (setf n2
              (if containers
                  (material-refractive-index
                   (shape-material
                    (first containers)))
                  1.0))
        (return)))
    ;; answer a computations
    (make-computations
     :tt tt
     :object object
     :point point
     :over-point (v+ point (v* normalv +epsilon+))
     :under-point (v- point (v* normalv +epsilon+))
     :eyev eyev
     :normalv normalv
     :inside inside
     :reflectv (reflect (ray-direction ray) normalv)
     :n1 n1
     :n2 n2)))

(defun is-shadowed (world point)
  (let* ((v (v- (light-position (world-light world)) point))
         (distance (vlength v))
         (direction (vunit v))
         (r (make-ray :origin point :direction direction))
         (is (intersect-world world r))
         (h (hit is)))
    (and h (< (intersektion-tt h) distance))))

(defun reflected-color (world comps remaining)
  (if (or (zerop remaining)
          (zerop (material-reflective (shape-material (computations-object comps)))))
      +black+
      (let* ((reflect-ray (make-ray :origin (computations-over-point comps)
                                    :direction (computations-reflectv comps)))
             (color (color-at world reflect-ray (1- remaining))))
        (v* color
            (material-reflective (shape-material
                                  (computations-object comps)))))))

(defun refracted-color (world comps remaining)
  "Answer the color thru transparent objects"
  (if (or (zerop
           (material-transparency
            (shape-material
             (computations-object comps))))
          (zerop remaining))
      +black+
      ;; total internal relection by Snell's law
      (let* ((n-ratio (/ (computations-n1 comps) (computations-n2 comps)))
             (cos-i (v. (computations-eyev comps) (computations-normalv comps)))
             (sin2-t (* n-ratio n-ratio (- 1 (* cos-i cos-i)))))
        (if (> sin2-t 1)
            +black+
            (let* ((cos-t (sqrt (- 1.0 sin2-t)))
                   (direction (v- (v* (computations-normalv comps)
                                      (- (* n-ratio cos-i) cos-t))
                                  (v* (computations-eyev comps) n-ratio)))
                   (refract-ray (make-ray :origin (computations-under-point comps)
                                          :direction direction)))
              (v* (color-at world refract-ray (1- remaining))
                  (material-transparency (shape-material (computations-object comps)))))))))

(defun schlick (comps)
  (flet ((r-expr (cos)
           (let* ((r0 (expt (/ (- (computations-n1 comps) (computations-n2 comps))
                              (+ (computations-n1 comps) (computations-n2 comps)))
                           2))
                 (cos1 (- 1 cos))
                 (cos2 (* cos1 cos1)))
             (+ r0 (* (- 1 r0) cos2 cos2 cos1)))))
    (let* ((cos (v. (computations-eyev comps) (computations-normalv comps))))
      (if (> (computations-n1 comps) (computations-n2 comps))
          (let* ((n (/ (computations-n1 comps) (computations-n2 comps)))
                 (sin2-t (* (* n n) (- 1.0 (* cos cos)))))
            (if (> sin2-t 1.0)
                1.0
                (r-expr (sqrt (- 1.0 sin2-t)))))
          (r-expr cos)))))

(defun shade-hit (world comps &optional (remaining 4))
  (let* ((shadowed (is-shadowed world (computations-over-point comps)))
         (surface (lighting (shape-material (computations-object comps))
                            (computations-object comps)
                            (world-light world)
                            (computations-over-point comps)
                            (computations-eyev comps)
                            (computations-normalv comps)
                            shadowed))
         (reflected (reflected-color world comps remaining))
         (refracted (refracted-color world comps remaining))
         (material (shape-material (computations-object comps))))
    (if (and (> (material-reflective material) 0.0)
             (> (material-transparency material) 0.0))
        (let ((reflectance (schlick comps)))
          (v+ surface
              (v* reflected reflectance)
              (v* refracted (- 1.0 reflectance))))
        (v+ surface reflected refracted))))

(defun color-at (world ray &optional (remaining 4))
  (let ((h (hit (intersect-world world ray))))
    (if h
        (shade-hit world (prepare-computations h ray) remaining)
        (color 0 0 0))))

