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
