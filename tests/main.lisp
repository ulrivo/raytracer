(defpackage raytracer/tests/main
  (:use :cl
        :raytracer
        :rove))
(in-package :raytracer/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :raytracer)' in your Lisp.

(deftest test-target-1
  (testing "should (= 2 1) to be true"
    (ok (= 1 2)))
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
