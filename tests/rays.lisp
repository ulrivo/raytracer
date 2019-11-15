(in-package :raytracer/tests/rays)

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

(deftest intersect
    (testing "intersection of sphere and ray"
             (ok (let* ((r (make-ray :origin (point 0 0 -5) :direction (vectorr 0 0 1)))
                        (s (make-sphere))
                        (xs (intersect s r)))
                   (and (= 2 (length xs))
                        (= 4 (intersektion-tt (first xs)))
                        (= 6 (intersektion-tt (second xs)))
                        (equalp s (intersektion-object (first xs)))
                        (equalp s (intersektion-object (second xs)))))))
  (testing "ray misses sphere"
           (ok (let* ((r (make-ray :origin (point 0 2 -5) :direction (vectorr 0 0 1)))
                      (s (make-sphere))
                      (xs (intersect s r)))
                 (zerop (length xs)))))
  (testing "ray originates inside the sphere"
           (ok (let* ((r (make-ray :origin (point 0 0 0) :direction (vectorr 0 0 1)))
                      (s (make-sphere))
                      (xs (intersect s r)))
                 (and (= 2 (length xs))
                      (= -1 (intersektion-tt (first xs)))
                      (= 1 (intersektion-tt (second xs)))))))
  (testing "sphere is behind a ray"
           (ok (let* ((r (make-ray :origin (point 0 0 5) :direction (vectorr 0 0 1)))
                      (s (make-sphere))
                      (xs (intersect s r)))
                 (and (= 2 (length xs))
                      (= -6 (intersektion-tt (first xs)))
                      (= -4 (intersektion-tt (second xs))))))))

(deftest hits
    (let ((s (make-sphere)))
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
               (ok (let* ((m (translation 3 4 5))
                          (r2 (transform ray m)))
                     (and (equalp (ray-origin r2) (point 4 6 8))
                          (equalp (ray-direction r2) (vectorr 0 1 0))))))
      (testing "scaling a ray"
               (ok (let* ((m (scaling 2 3 4))
                          (r2 (transform ray m)))
                     (and (equalp (ray-origin r2) (point 2 6 12))
                          (equalp (ray-direction r2) (vectorr 0 3 0))))))))

(deftest transform-sphere
    (let ((ray (make-ray :origin (point 0 0 -5) :direction (vectorr 0 0 1))))
      (testing "intersecting a scaled sphere with a ray"
               (ok (let* ((s (make-sphere :transform (scaling 2 2 2)))
                          (xs (intersect s ray)))
                     (princ xs)
                     (and (= 2 (length xs))
                          (= 3 (intersektion-tt (first xs)))
                          (= 7 (intersektion-tt (second xs)))))))))

(deftest normal-on-sphere
    (let ((s (make-sphere)))
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
             (ok (let* ((s (make-sphere :transform (translation 0 1 0)))
                        (n (normal-at s (point 0 1.70711 -0.70711))))
                   (approximately n (vectorr 0 0.70711 -0.70711)))))
  (testing "normal on a transformed sphere"
           (ok (let* ((m (mm (scaling 1 0.5 1) (rotation-z (/ pi 5))))
                      (s (make-sphere :transform m))
                      (n (normal-at s (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))))))
                 (approximately n (vectorr 0 0.97014 -0.24254))))))

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

(deftest material
    (let ((m (make-material))
          (position (point 0 0 0)))
      (testing "lighting with the eye between the light and the surface"
               (ok (let* ((eyev (vectorr 0 0 -1))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 0 -10)))
                          (result (lighting m light position eyev normalv nil)))
                     (equalp result (color 1.9 1.9 1.9)))))
      (testing "lighting with the eye between the light and the surface, eye offset 45 degree"
               (ok (let* ((eyev (vectorr 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 0 -10)))
                          (result (lighting m light position eyev normalv nil)))
                     (equalp result (color 1 1 1)))))
      (testing "lighting with the eye opposite surface, eye offset 45 degree"
               (ok (let* ((eyev (vectorr 0 0 -1))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 10 -10)))
                          (result (lighting m light position eyev normalv nil)))
                     (approximately result (color 0.7364 0.7364 0.7364)))))
      (testing "lighting with the eye in the path of the reflection vector"
               (ok (let* ((sq2 (- (/ (sqrt 2) 2)))
                          (eyev (vectorr 0 sq2 sq2))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 10 -10)))
                          (result (lighting m light position eyev normalv nil)))
                     (approximately result (color 1.6363853 1.6363853 1.6363853)))))
      (testing "lighting with the eye behind the surface"
               (ok (let* ((eyev (vectorr 0 0 -1))
                          (normalv (vectorr 0 0 -1))
                          (light (make-light :position (point 0 0 10)))
                          (result (lighting m light position eyev normalv nil)))
                   (approximately result (color 0.1 0.1 0.1)))))))

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
            (shape (make-sphere))
            (i (make-intersektion :tt 4 :object shape)))
       (computations-inside (prepare-computations i r)))))

  (testing "prepare computations"
    (let* ((r (make-ray
               :origin (point 0 0 0)
               :direction (vectorr 0 0 1)))
           (shape (make-sphere))
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
          (shape (make-sphere :transform (translation 0 0 1)))
          (i (make-intersektion :tt 5 :object shape))
           (comps (prepare-computations i r)))
      (ok (< (aref (computations-over-point comps) 2)
             (- (/ +epsilon+ 2))))
      (ok (> (aref (computations-point comps) 2)
             (aref (computations-over-point comps) 2))))))


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
           (ok (let* ((s1 (make-sphere))
                      (s2 (make-sphere :transform (translation 0 0 10)))
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
                         (shade-hit w comps))))))

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
          (setf (material-ambient (sphere-material outer)) 1)
          (setf (material-ambient (sphere-material inner)) 1)
          (approximately (color-at w r)
                         (material-colour (sphere-material inner)))))))

(deftest lighting-in-shadow
    (testing "lighting with the surface in shadow"
      (ok (let ((m (make-material))
                (eyev (vectorr 0 0 -1))
                (normalv (vectorr 0 0 -1))
                (light (make-light :position (point 0 0 -10)
                                  :intensity (color 1 1 1)))
                (in-shadow t))
            (equalp (color 0.1 0.1 0.1)
                    (lighting m light (point 0 0 0)
                              eyev normalv in-shadow))))))
