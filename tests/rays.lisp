(in-package :raytracer/tests/rays)

(use-package :3d-matrices)

;; NOTE: To run this test file, execute `(asdf:test-system :raytracer)'
;; in your Lisp.

(deftest ray-point
    (testing "compute a point from ray"
             (let ((ray (make-ray :origin (point 2 3 4) :direction (vectorr 1 0 0))))
               (ok (equalp
                    (ray-position ray 0)
                    (point 2 3 4)))
               (ok (equalp
                    (ray-position ray 1)
                    (point 3 3 4)))
               (ok (equalp
                    (ray-position ray -1)
                    (point 1 3 4)))
               (ok (equalp
                    (ray-position ray 2.5)
                    (point 4.5 3 4))))))

(deftest view-transform
    (testing "The transformation matrix for the default orientation"
             (ok (equalp +identity-matrix+
                         (view-transform (point 0 0 0)
                                         (point 0 0 -1)
                                         (vectorr 0 1 0)))))
  (testing "A view transformation matrix looking in positive z direction"
           (ok (equalp (scaling -1 1 -1)
                       (view-transform (point 0 0 0)
                                       (point 0 0 1)
                                       (vectorr 0 1 0)))))
  (testing "A view transformation matrix looking in positive z direction"
           (ok (equalp (translation 0 0 -8)
                       (view-transform (point 0 0 8)
                                       (point 0 0 0)
                                       (vectorr 0 1 0)))))
  (testing "An arbitrary view transformation"
           (ok (approximately (mat
                        -0.50709  0.50709   0.67612  -2.36643
                        0.76772  0.60609   0.12122  -2.82843
                        -0.35857  0.59761  -0.71714   0.00000
                        0.00000  0.00000   0.00000   1.00000)
                       (view-transform (point 1 3 2)
                                       (point 4 -2 8)
                                       (vectorr 1 1 0))))))

(deftest intersect
    (testing "intersection of sphere and ray"
             (let* ((r (make-ray :origin (point 0 0 -5) :direction (vectorr 0 0 1)))
                    (s (default-sphere))
                    (xs (intersect s r)))
               (ok (= 2 (length xs)))
               (ok (= 4 (intersektion-tt (first xs))))
               (ok (= 6 (intersektion-tt (second xs))))
               (ok (eq s (intersektion-object (first xs))))
               (ok (eq s (intersektion-object (second xs))))))
  (testing "ray misses sphere"
           (ok (let* ((r (make-ray :origin (point 0 2 -5) :direction (vectorr 0 0 1)))
                      (s (default-sphere))
                      (xs (intersect s r)))
                 (zerop (length xs)))))
  (testing "ray originates inside the sphere"
           (let* ((r (make-ray :origin (point 0 0 0) :direction (vectorr 0 0 1)))
                       (s (default-sphere))
                       (xs (intersect s r)))
             (ok (= 2 (length xs)))
             (ok (= -1 (intersektion-tt (first xs))))
             (ok (= 1 (intersektion-tt (second xs))))))
  (testing "sphere is behind a ray"
           (let* ((r (make-ray :origin (point 0 0 5) :direction (vectorr 0 0 1)))
                       (s (default-sphere))
                       (xs (intersect s r)))
             (ok (= 2 (length xs)))
             (ok (= -6 (intersektion-tt (first xs))))
             (ok (= -4 (intersektion-tt (second xs))))))
  (testing "Intersect with a ray parallel to the plane"
           (let ((p (default-plane))
                 (r (make-ray :origin (point 0 10 0) :direction (vectorr 0 0 1))))
             (ng (intersect p r))))
  (testing "Intersect with a coplanar ray"
           (let ((p (default-plane))
                 (r (make-ray :origin (point 0 0 0) :direction (vectorr 0 0 1))))
             (ng (intersect p r))))
  (testing "A ray intersecting a plane from above"
           (let* ((p (default-plane))
                 (r (make-ray :origin (point 0 1 0) :direction (vectorr 0 -1 0)))
                 (xs (intersect p r)))
             (ok (= 1 (length xs)))
             (ok (= 1 (intersektion-tt (first xs))))
             (ok (eq p (intersektion-object (first xs))))))
  (testing "A ray intersecting a plane from below"
           (let* ((p (default-plane))
                 (r (make-ray :origin (point 0 -1 0) :direction (vectorr 0 1 0)))
                 (xs (intersect p r)))
             (ok (= 1 (length xs)))
             (ok (= 1 (intersektion-tt (first xs))))
             (ok (eq p (intersektion-object (first xs))))))
  (testing "A ray intersects a cube"
           (let* ((c (default-cube)))
             (mapc (lambda (origin direction t1 t2)
                     (let* ((r (make-ray :origin origin :direction direction))
                            (xs (intersect c r)))
                       (ok (= 2 (length xs)))
                       (ok (= t1 (intersektion-tt (first xs))))
                       (ok (= t2 (intersektion-tt (second xs))))))
                   (list (point 5 0.5 0)
                         (point -5 0.5 0)
                         (point 0.5 5 0)
                         (point 0.5 -5 0)
                         (point 0.5 0 5)
                         (point 0.5 0 -5)
                         (point 0 0.5 0))
                   (list (vectorr -1 0 0)
                         (vectorr 1 0 0)
                         (vectorr 0 -1 0)
                         (vectorr 0 1 0)
                         (vectorr 0 0 -1)
                         (vectorr 0 0 1)
                         (vectorr 0 0 1))
                   '(4 4 4 4 4 4 -1)
                   '(6 6 6 6 6 6 1))))
  (testing "A ray misses a cube"
    (let ((c (default-cube)))
      (mapc (lambda (origin direction)
              (let ((r (make-ray :origin origin
                                 :direction direction)))
                (ng (intersect c r))))
            (list (point -2 0 0)
                  (point 0 -2 0)
                  (point 0 0 -2)
                  (point 2 0 2)
                  (point 0 2 2)
                  (point 2 2 0))
            (list (vectorr 0.2673 0.5345 0.8018)
                  (vectorr 0.8018 0.2673 0.5345)
                  (vectorr 0.5345 0.8018 0.2673)
                  (vectorr 0 0 -1)
                  (vectorr 0 -1 0)
                  (vectorr -1 0 0))))))

(deftest hits
    (let ((s (default-sphere)))
      (testing "all intersections have positive tt"
               (ok (let* ((i1 (make-intersektion :tt 1 :object s))
                          (i2 (make-intersektion :tt 2 :object s))
                          (xs (list i2 i1))
                          (i (hit xs)))
                     (equal i i1))))
      (testing "some intersections have negative tt"
               (ok (let* ((i1 (make-intersektion :tt -1 :object s))
                          (i2 (make-intersektion :tt 1 :object s))
                          (xs (list i2 i1))
                          (i (hit xs)))
                     (equal i i2))))
      (testing "all intersetions have negative tt"
               (ok (let* ((i1 (make-intersektion :tt -1 :object s))
                          (i2 (make-intersektion :tt -2 :object s))
                          (xs (list i2 i1))
                          (i (hit xs)))
                     (null i))))
      (testing "the hit is always the lowest nonnegative intersection" 
               (ok (let* ((i1 (make-intersektion :tt 5 :object s))
                          (i2 (make-intersektion :tt 7 :object s))
                          (i3 (make-intersektion :tt -3 :object s))
                          (i4 (make-intersektion :tt 2 :object s))
                          (xs (list i4 i3 i2 i1))
                          (i (hit xs)))
                     (equal i i4))))))

(deftest moving-ray
    (let ((ray (make-ray :origin (point 1 2 3) :direction (vectorr 0 1 0))))
      (testing "translating a ray"
        (ok (let* ((m (mtranslation (vec 3 4 5)))
                          (r2 (transform ray m)))
                     (and (equalp (ray-origin r2) (point 4 6 8))
                          (equalp (ray-direction r2) (vectorr 0 1 0))))))
      (testing "scaling a ray"
               (ok (let* ((m (mscaling (vec 2 3 4)))
                          (r2 (transform ray m)))
                     (and (equalp (ray-origin r2) (point 2 6 12))
                          (equalp (ray-direction r2) (vectorr 0 3 0))))))))

(deftest transform-sphere
    (let ((ray (make-ray :origin (point 0 0 -5) :direction (vectorr 0 0 1))))
      (testing "intersecting a scaled sphere with a ray"
        (let* ((s (make-sphere (mscaling (vec 2 2 2)) (make-material)))
                    (xs (intersect s ray)))
               (princ xs)
          (ok (= 2 (length xs)))
          (ok (= 3 (intersektion-tt (first xs))))
          (ok (= 7 (intersektion-tt (second xs))))))))

(deftest normal-on-sphere
    (let ((s (default-sphere)))
      (testing "normal on sphere at a point on the x axis"
               (ok (let ((n (normal-at s (point 1 0 0))))
                     (equalp n (vectorr 1 0 0)))))
      (testing "normal on sphere at a point on the y axis"
               (ok (let ((n (normal-at s (point 0 1 0))))
                     (equalp n (vectorr 0 1 0)))))
      (testing "normal on sphere at a point on the z axis"
               (ok (let ((n (normal-at s (point 0 0 1))))
                     (equalp n (vectorr 0 0 1)))))
      (testing "normal on sphere at a nonaxial point on the x axis"
               (ok (let* ((sq3 (/ (sqrt 3) 3))
                          (n (normal-at s (point sq3 sq3 sq3))))
                     (approximately n (vectorr sq3 sq3 sq3)))))))

(deftest normal-on-transformed-sphere
    (testing "normal on a translated sphere"
      (ok (let* ((s (make-sphere (mtranslation (vec  0 1 0)) (make-material)))
                        (n (normal-at s (point 0 1.70711 -0.70711))))
                   (approximately n (vectorr 0 0.70711 -0.70711)))))
  (testing "normal on a transformed sphere"
    (ok (let* ((m (m* (mscaling (vec 1 0.5 1)) (mrotation (vec 0 0 1) (/ pi 5))))
                      (s (make-sphere m (make-material)))
                      (n (normal-at s (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))))))
                 (approximately n (vectorr 0 0.97014 -0.24254))))))

(deftest normal-on-plane
    (testing "The normal of a plane is constant everywhere"
             (let* ((p (default-plane))
                    (n1 (normal-at p (point 0 0 0)))
                    (n2 (normal-at p  (point 10 0 -10)))
                    (n3 (normal-at p (point -5 0 150)))
                    (ok (equalp n1 (vectorr 0 1 0)))
                    (ok (equalp n2 (vectorr 0 1 0)))
                    (ok (equalp n3 (vectorr 0 1 0)))))))

(deftest normal-on-cube
  (testing "the normal on the surface of a cube"
    (let ((c (default-cube)))
      (mapc (lambda (point normal)
              (ok (equalp normal (normal-at c point))))
            (list (point 1 0.5 -0.8)
                  (point -1 -0.2 0.9)
                  (point -0.4 1 -0.1)
                  (point 0.3 -1 -0.7)
                  (point -0.6 0.3 1)
                  (point 0.4 0.4 -1)
                  (point 1 1 1)
                  (point -1 -1 -1))
            (list (vectorr 1 0 0)
                  (vectorr -1 0 0)
                  (vectorr 0 1 0)
                  (vectorr 0 -1 0)
                  (vectorr 0 0 1)
                  (vectorr 0 0 -1)
                  (vectorr 1 0 0)
                  (vectorr -1 0 0))))))

(deftest reflection
    (testing "reflect a vector approaching at 45 degree"
             (ok (let* ((v (vectorr 1 -1 0))
                        (n (vectorr 0 1 0))
                        (r (reflect v n)))
                   (equalp r (vectorr 1 1 0)))))
  (testing "reflect off a slanted surface" 
           (ok (let* ((v (vectorr 0 -1 0))
                      (sq2 (/ (sqrt 2) 2))
                      (n (vectorr sq2 sq2 0))
                      (r (reflect v n)))
                 (approximately r (vectorr 1 0 0))))))

(deftest test-pattern
  (let ((pattern (make-test-pattern)))
    (testing "A pattern with an object transformation"
      (let* ((shape (make-sphere
                     (scaling 2 2 2)
                     (make-material)))
             (c (pattern-at-shape pattern shape (point 2 3 4))))
        (ok (equalp c (color 1 1.5 2)))))))

(deftest pattern
  (testing "A gradient linearly interpolates between colors"
    (let ((pattern (make-gradient-pattern +white+ +black+ +identity-matrix+)))
      (ok (equalp (pattern-at pattern (point 0 0 0)) +white+))))
  (testing "A ring should extend in both x and z"
    (let ((pattern (make-ring-pattern +white+ +black+ +identity-matrix+)))
      (ok (equalp (pattern-at pattern (point 0 0 0)) +white+))
      (ok (equalp (pattern-at pattern (point 0 0 1)) +black+))
      (ok (equalp (pattern-at pattern (point 0.708 0 0.708)) +black+))
      (ok (equalp (pattern-at pattern (point 1 0 0)) +black+)))))

(deftest material
    (let ((m (make-material))
          (position (point 0 0 0)))
      (testing "lighting with the eye between the light and the surface"
               (ok (let* ((eyev (vectorr 0 0 -1))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 0 -10)))
                          (result (lighting m (default-sphere) light position eyev normalv nil)))
                     (equalp result (color 1.9 1.9 1.9)))))
      (testing "lighting with the eye between the light and the surface, eye offset 45 degree"
               (ok (let* ((eyev (vectorr 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 0 -10)))
                          (result (lighting m (default-sphere) light position eyev normalv nil)))
                     (equalp result (color 1 1 1)))))
      (testing "lighting with the eye opposite surface, eye offset 45 degree"
               (ok (let* ((eyev (vectorr 0 0 -1))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 10 -10)))
                          (result (lighting m (default-sphere) light position eyev normalv nil)))
                     (approximately result (color 0.7364 0.7364 0.7364)))))
      (testing "lighting with the eye in the path of the reflection vector"
               (ok (let* ((sq2 (- (/ (sqrt 2) 2)))
                          (eyev (vectorr 0 sq2 sq2))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 10 -10)))
                          (result (lighting m (default-sphere) light position eyev normalv nil)))
                     (approximately result (color 1.6363853 1.6363853 1.6363853)))))
      (testing "lighting with the eye behind the surface"
               (ok (let* ((eyev (vectorr 0 0 -1))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 0 10)))
                          (result (lighting m (default-sphere) light position eyev normalv nil)))
                     (approximately result (color 0.1 0.1 0.1)))))))

(deftest lighting-pattern
    (testing "lighting with a pattern applied"
             (let ((m (make-material
                       :pattern (make-stripe-pattern +white+ +black+ +identity-matrix+)
                       :ambient 1
                       :diffuse 0
                       :specular 0))
                   (eyev (vectorr 0 0 -1))
                   (normalv (vectorr 0 0 -1))
                   (light (make-light :position (point 0 0 -10)
                                      :intensity (color 1 1 1))))
               (ok (equalp (lighting m (default-sphere) light (point 0.9 0 0) eyev normalv nil)
                           +white+))
               (ok (equalp (lighting m (default-sphere) light (point 1.1 0 0) eyev normalv nil)
                           +black+)))))

(deftest intersect-world
    (testing "intersect a world with a ray"
             (let* ((w (default-world))
                        (r (make-ray :origin (point 0 0 -5)
                                     :direction (vectorr 0 0 1)))
                    (xs (intersect-world w r)))
               (ok (= (length xs) 4))
               (ok (= (intersektion-tt (nth 0 xs)) 4))
               (ok (= (intersektion-tt (nth 1 xs)) 4.5))
               (ok (= (intersektion-tt (nth 2 xs)) 5.5))
               (ok (= (intersektion-tt (nth 3 xs)) 6)))))

(deftest prep-comps
  (testing "the hit, when an intersection occurs on the outside"
    (ng
     (let* ((r (make-ray :origin (point 0 0 -5) :direction (vectorr 0 0 1)))
            (shape (default-sphere))
            (i (make-intersektion :tt 4 :object shape)))
       (computations-inside (prepare-computations i r)))))

  (testing "prepare computations"
    (let* ((r (make-ray
               :origin (point 0 0 0)
               :direction (vectorr 0 0 1)))
           (shape (default-sphere))
           (i (make-intersektion :tt 1 :object shape))
           (comps (prepare-computations i r)))
      (ok (eql (computations-tt comps) (intersektion-tt i)))
      (ok     (equalp (computations-object comps) (intersektion-object i)))
      (ok     (equalp (computations-point comps) (point 0 0 1)))
      (ok    (computations-inside comps))
      (ok (equalp (computations-eyev comps) (vectorr 0 0 -1)))
      (ok  (equalp (computations-normalv comps) (vectorr 0 0 -1)))))

  (testing "the hit should offset the point"
    (let* ((r (make-ray :origin (point 0 0 -5)
                        :direction (vectorr 0 0 1)))
           (shape (make-sphere (mtranslation (vec 0 0 1)) (make-material)))
          (i (make-intersektion :tt 5 :object shape))
           (comps (prepare-computations i r)))
      (ok (< (vz (computations-over-point comps))
             (- (/ +epsilon+ 2))))
      (ok (> (vz (computations-point comps))
             (vz (computations-over-point comps))))))

  (testing "Finding n1 and n2 at various intersections"
    (let* ((a (glass-sphere (scaling 2 2 2) 1.5))
           (b (glass-sphere (translation 0 0 -0.25) 2.0))
           (c (glass-sphere (translation 0 0 0.25) 2.5))
           (r (make-ray :origin (point 0 0 -4)
                        :direction (vectorr 0 0 1)))
           (xs (mapcar (lambda (x y)
                         (make-intersektion :tt x :object y))
                       '(2.0 2.75 3.25 4.75 5.25 6.0)
                       (list a b c b c a))))
      (mapc (lambda (x n1 n2)
              (let ((c (prepare-computations x r xs)))
                (ok (= (computations-n1 c) n1))
                (ok (= (computations-n2 c) n2))))
            xs
            '(1.0 1.5 2.0 2.5 2.5 1.5)
            '(1.5 2.0 2.5 2.5 1.5 1.0))))

  (testing "The under point is offset below the surface"
    (let* ((r (make-ray :origin (point 0 0 -5)
                       :direction (vectorr 0 0 1)))
          (shape (glass-sphere (translation 0 0 1)))
          (i (make-intersektion :tt 5 :object shape))
          (xs (list i))
          (comps (prepare-computations i r xs)))
      (ok (> (vz (computations-under-point comps))
             (/ +epsilon+ 2)))
      (ok (< (vz (computations-point comps))
             (vz (computations-under-point comps)))))))

(deftest refracted-color
    (testing "The refracted color with an opaque surface"
             (let* ((w (default-world))
                    (shape (first (world-shapes w)))
                    (r (make-ray :origin (point 0 0 -5)
                                 :direction (vectorr 0 0 1)))
                    (xs (list (make-intersektion :tt 4 :object shape)
                              (make-intersektion :tt 6 :object shape)))
                    (comps (prepare-computations (first xs) r xs))
                    (c (refracted-color w comps 5)))
               (ok (equalp c +black+))))
  (testing "The refracted color at the maximum recursive depth"
           (let* ((w (make-world
                      :light (make-light
                              :position (point -10 10 -10)
                              :intensity (color 1 1 1))
                      :shapes (list
                               (make-sphere
                                +identity-matrix+
                                (make-material
                                 :colour (color 0.8 1.0 0.6)
                                 :diffuse 0.7
                                 :specular 0.2
                                 :transparency 1.0
                                 :refractive-index 1.5))
                               (make-sphere
                                (mscaling (vec 0.5 0.5 0.5))
                                (make-material)))))
                  (shape (first (world-shapes w)))
                  (r (make-ray :origin (point 0 0 -5)
                               :direction (vectorr 0 0 1)))
                  (xs (list (make-intersektion :tt 4 :object shape)
                            (make-intersektion :tt 6 :object shape)))
                  (comps (prepare-computations (first xs) r xs))
                  (c (refracted-color w comps 0)))
             (ok (equalp c +black+))))
  (testing "The refracted color under total internal reflection"
           (let* ((w (make-world
                      :light (make-light
                              :position (point -10 10 -10)
                              :intensity (color 1 1 1))
                      :shapes (list
                               (make-sphere
                                +identity-matrix+
                                (make-material
                                 :colour (color 0.8 1.0 0.6)
                                 :diffuse 0.7
                                 :specular 0.2
                                 :transparency 1.0
                                 :refractive-index 1.5))
                               (make-sphere
                                (mscaling (vec 0.5 0.5 0.5))
                                (make-material)))))
                  (shape (first (world-shapes w)))
                  (sqr (/ (sqrt 2) 2))
                  (r (make-ray :origin (point 0 0 sqr)
                               :direction (vectorr 0 1 0)))
                  (xs (list (make-intersektion :tt (- sqr) :object shape)
                            (make-intersektion :tt sqr :object shape)))
                  (comps (prepare-computations (second xs) r xs))
                  (c (refracted-color w comps 5)))
             (ok (equalp c +black+))))
  (testing "The refracted color with a refracted ray"
    (let* ((a (make-sphere
               +identity-matrix+
               (make-material
                :colour (color 0.8 1.0 0.6)
                :diffuse 0.7
                :specular 0.2
                :ambient 1.0
                :pattern (make-test-pattern))))
           (b (make-sphere
               (mscaling (vec 0.5 0.5 0.5))
               (make-material
                :transparency 1.0
                :refractive-index 1.5)))
           (w (make-world
                      :light (make-light
                              :position (point -10 10 -10)
                              :intensity (color 1 1 1))
                      :shapes (list a b)))
                  (r (make-ray :origin (point 0 0 0.1)
                               :direction (vectorr 0 1 0)))
                  (xs (list (make-intersektion :tt -0.9899 :object a)
                            (make-intersektion :tt -0.4899 :object b)
                            (make-intersektion :tt  0.4899 :object b)
                            (make-intersektion :tt  0.9899 :object a)))
                  (comps (prepare-computations (third xs) r xs))
                  (c (refracted-color w comps 5)))
      (ok (approximately  c (color 0 0.99888 0.04725))))))

(deftest shade-hit
  (testing "shading an intersection"
    (let* ((w (default-world))
           (r (make-ray
               :origin (point 0 0 -5)
               :direction (vectorr 0 0 1)))
           (shape (first (world-shapes w)))
           (i (make-intersektion :tt 4 :object shape))
           (comps (prepare-computations i r))
           (c (shade-hit w comps)))
      (ok (approximately c (color 0.38066 0.47583 0.2855)))))

  (testing "shading an intersection from the inside"
    (let* ((w (default-world))
           (r (make-ray
               :origin (point 0 0 0)
               :direction (vectorr 0 0 1)))
           (shape (second (world-shapes w)))
           (i (make-intersektion :tt 0.5 :object shape))
           (comps (prepare-computations i r)))
      (setf (world-light w) (make-light
                             :position (point 0 0.25 0)
                             :intensity (color 1 1 1)))
      (ok (approximately (shade-hit w comps) (color 0.90498 0.90498 0.90498)))))

  (testing "shade_hit() is given an intersection in shadow"
           (ok (let* ((s1 (default-sphere))
                      (s2 (make-sphere (mtranslation (vec 0 0 10)) (make-material)))
                      (w (make-world
                          :light (make-light
                                  :position (point 0 0 -10)
                                  :intensity (color 1 1 1))
                          :shapes (list s1 s2)))
                      (r (make-ray :origin (point 0 0 5)
                                   :direction (vectorr 0 0 1)))
                      (i (make-intersektion :tt 4
                                            :object s2))
                      (comps (prepare-computations i r)))
                 (equalp (color 0.1 0.1 0.1)
                         (shade-hit w comps)))))

  (testing "shade-hit with a reflective material"
    (ok (let* ((w (default-world))
               (sqr (/ (sqrt 2) 2))
               (r (make-ray :origin (point 0 0 -3)
                            :direction (vectorr 0 (- sqr) sqr )))
               (shape (make-plane
                       (translation 0 -1 0)
                       (make-material :reflective 0.5))))
          (push shape (world-shapes w))
          (let* ((i (make-intersektion :tt (sqrt 2) :object shape))
                 (comps (prepare-computations i r))
                 (color (shade-hit w comps)))
            (approximately color
                           (color 0.87677 0.92436 0.82918))))))

  (testing "shade_hit() with a transparent material"
           (let* ((w (default-world))
                  (floor (make-plane
                           (translation 0 -1 0)
                           (make-material
                            :transparency 0.5
                            :refractive-index 1.5)))
                  (ball (make-sphere
                         (translation 0 -3.5 -0.5)
                         (make-material
                          :colour (color 1 0 0)
                          :ambient 0.5)))
                  (sqr (/ (sqrt 2) 2))
                  (r (make-ray :origin (point 0 0 -3)
                                 :direction (vectorr 0 (- sqr) sqr)))
                  (xs (list (make-intersektion :tt (sqrt 2) :object floor)))
                  (comps (prepare-computations (first xs) r xs)))
             (push floor (world-shapes w))
             (push ball (world-shapes w))
             (ok (approximately (color 0.93642 0.68642 0.68642 )
                                (shade-hit w comps 5)))))

  (testing "shade_hit() with a reflective transparent material"
           (let* ((w (default-world))
                  (floor (make-plane
                           (translation 0 -1 0)
                           (make-material
                            :transparency 0.5
                            :reflective 0.5
                            :refractive-index 1.5)))
                  (ball (make-sphere
                         (translation 0 -3.5 -0.5)
                         (make-material
                          :colour (color 1 0 0)
                          :ambient 0.5)))
                  (sqr (/ (sqrt 2) 2))
                  (r (make-ray :origin (point 0 0 -3)
                                 :direction (vectorr 0 (- sqr) sqr)))
                  (xs (list (make-intersektion :tt (sqrt 2) :object floor)))
                  (comps (prepare-computations (first xs) r xs)))
             (push floor (world-shapes w))
             (push ball (world-shapes w))
             (ok (approximately (color 0.93391 0.69643 0.69243)
                                (shade-hit w comps 5))))))

(deftest color-at
  (testing "the color when a ray misses"
    (ok (let ((w (default-world))
              (r (make-ray :origin (point 0 0 -5)
                           :direction (vectorr 0 1 0))))
          (equalp (color-at w r) (color 0 0 0)))))

  (testing "the color when a ray hits"
    (ok (let ((w (default-world))
              (r (make-ray :origin (point 0 0 -5)
                           :direction (vectorr 0 0 1))))
          (approximately (color-at w r)
                         (color 0.38066 0.47583 0.2855)))))

  (testing "the color with an intersection behind the ray"
    (ok (let* ((w (default-world))
               (outer (first (world-shapes w)))
               (inner (second (world-shapes w)))
              (r (make-ray :origin (point 0 0 0.75)
                           :direction (vectorr 0 0 -1))))
          (setf (material-ambient (shape-material outer)) 1)
          (setf (material-ambient (shape-material inner)) 1)
          (approximately (color-at w r)
                         (material-colour (shape-material inner)))))))

(deftest lighting-in-shadow
    (testing "lighting with the surface in shadow"
      (ok (let ((m (make-material))
                (eyev (vectorr 0 0 -1))
                (normalv (vectorr 0 0 -1))
                (light (make-light :position (point 0 0 -10)
                                  :intensity (color 1 1 1)))
                (in-shadow t))
            (equalp (color 0.1 0.1 0.1)
                    (lighting m (default-sphere) light (point 0 0 0)
                              eyev normalv in-shadow))))))

(deftest schlick
    (testing "The Schlick approximation under total internal reflection"
             (let* ((shape (glass-sphere))
                    (sqr (/ (sqrt 2) 2))
                    (r (make-ray :origin (point 0 0 sqr)
                                 :direction (vectorr 0 1 0)))
                    (xs (list (make-intersektion :tt (- sqr) :object shape)
                              (make-intersektion :tt sqr :object shape)))
                    (comps (prepare-computations (second xs) r xs)))
               (ok (= (schlick comps) 1))))
  (testing "The Schlick approximation with a perpendicular viewing angle"
           (let* ((shape (glass-sphere))
                  (r (make-ray :origin (point 0 0 0)
                               :direction (vectorr 0 1 0)))
                  (xs (list (make-intersektion :tt -1 :object shape)
                            (make-intersektion :tt 1 :object shape)))
                  (comps (prepare-computations (second xs) r xs)))
             (ok (approximately (schlick comps) 0.04))))
  (testing "The Schlick approximation with small angle and n2 > n1"
             (let* ((shape (glass-sphere))
                    (r (make-ray :origin (point 0 0.99 -2)
                                 :direction (vectorr 0 0 1)))
                    (xs (list (make-intersektion :tt 1.8589 :object shape)))
                    (comps (prepare-computations (first xs) r xs)))
               (ok (approximately (schlick comps) 0.48873)))))
