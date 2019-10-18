(defpackage raytracer/tests/rays
  (:use :cl
   :raytracer
        :rove))
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
