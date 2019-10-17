(defpackage raytracer/tests/main
  (:use :cl
        :raytracer
        :rove))
(in-package :raytracer/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :raytracer)' in your Lisp.

(deftest matrices
  (testing "correct matrix multiplication"
    (ok (equalp (mm
                 (make-array '(4 4) :initial-contents
                             '((1 2 3 4)
                               (5 6 7 8)
                               (9 8 7 6)
                               (5 4 3 2)))
                 (make-array '(4 4) :initial-contents
                             '((-2 1 2 3)
                               (3 2 1 -1)
                               (4 3 6 5)
                               (1 2 7 8))))
                (make-array '(4 4) :initial-contents
                            '((20 22 50 48)
                              (44 54 114 108)
                              (40 58 110 102)
                              (16 26 46 42))))))
  (testing "error with incompatible matrices"
    (ok (signals (raytracer:mm
                  (make-array '(4 4) :initial-contents
                              '((1 2 3 4)
                                (5 6 7 8)
                                (9 8 7 6)
                                (5 4 3 2)))
                  (make-array '(3 4) :initial-contents
                              '((-2 1 2 3)
                                (4 3 6 5)
                                (1 2 7 8))))))))
