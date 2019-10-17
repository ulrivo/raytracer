(defpackage raytracer/tests/matrices
  (:use :cl
        :raytracer
        :rove))
(in-package :raytracer/tests/matrices)

;; NOTE: To run this test file, execute `(asdf:test-system :raytracer)'
;; in your Lisp.

(deftest matrix-multiplication
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
  (testing "correct matrix and vector multiplication"
    (ok (equalp (mm
                 (make-array '(4 4) :initial-contents
                             '((1 2 3 4)
                               (2 4 4 2)
                               (8 6 4 1)
                               (0 0 0 1)))
                 (make-array '(4 1) :initial-contents
                             '((1) (2) (3) (1))))
                (make-array '(4 1) :initial-contents
                            '((18) (24) (33) (1))))))
  (testing "error with incompatible matrices"
    (ok (signals (mm
                  (make-array '(4 4) :initial-contents
                              '((1 2 3 4)
                                (5 6 7 8)
                                (9 8 7 6)
                                (5 4 3 2)))
                  (make-array '(3 4) :initial-contents
                              '((-2 1 2 3)
                                (4 3 6 5)
                                (1 2 7 8))))))))

(deftest determinant
  (testing "determinant of 2x2-matrix"
    (ok (eql 17
             (determinant
              (make-array '(2 2)
                          :initial-contents
                          '((1 5)
                            (-3 2)))))))
  (testing "determinant of 3x3-matrix"
    (ok (eql -196 
             (determinant
              (make-array '(3 3)
                          :initial-contents
                          '((1 2 6)
                            (-5 8 -4)
                            (2 6 4)))))))
    (testing "determinant of 4x4-matrix"
             (ok (eql -4071
                      (determinant
                       (make-array '(4 4)
                                   :initial-contents
                                   '((-2 -8 3 5)
                                     (-3 1 7 3)
                                     (1 2 -9 6)
                                     (-6 7 7 -9))))))))

(deftest submatrix
  (testing "part of matrices"
    (ok (equalp
         (submatrix (make-array '(3 3)
                                :initial-contents
                                '((1 5 0)
                                  (-3 2 7)
                                  (0 6 -3)))
                    0 2)
         (make-array '(2 2)
                     :initial-contents
                     '((-3 2)
                       (0 6)))))))

(deftest minor
  (testing "determinant of minor matrix"
    (ok (eql
         (minor (make-array '(3 3)
                            :initial-contents
                            '((3 5 0)
                              (2 -1 -7)
                              (6 -1 5)))
                1 0)
         25))))

(deftest cofactor
  (testing "minor matrix with the right sign"
    (ok (eql
         (minor (make-array '(3 3)
                            :initial-contents
                            '((3 5 0)
                              (2 -1 -7)
                              (6 -1 5)))
                1 0)
         25)))
  (testing "minor matrix with the right sign"
    (ok (eql
         (minor (make-array '(3 3)
                            :initial-contents
                            '((3 5 0)
                              (2 -1 -7)
                              (6 -1 5)))
                0 0)
         -12))))

(defun 2d-array-to-list (array)
  (apply #'append 
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j)))))

(deftest inverse
  (testing "error when determinant is zero"
    (ok (signals
         (inverse (make-array '(3 3)
                            :initial-contents
                            '((0 0 0)
                              (2 -1 -7)
                              (6 -1 5)))))))
  (testing "inverse of a 4x4-matrix"
    (ok (approximately
         (2d-array-to-list (inverse (make-array '(4 4)
                            :initial-contents
                            '((-5 2 6 -8)
                              (1 -5 1 8)
                              (7 7 -6 -7)
                              (1 -3 7 4)))))
         (2d-array-to-list (make-array '(4 4)
                     :initial-contents
                     '(( 0.21805 0.45113 0.24060 -0.04511 )
                       ( -0.80827 -1.45677 -0.44361 0.52068 )
                       ( -0.07895 -0.22368 -0.05263 0.19737 )
                       ( -0.52256 -0.81391 -0.30075 0.30639 ))))))))
