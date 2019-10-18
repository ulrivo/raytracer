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
                 (point 1 2 3))
                (point 18 24 33))))
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
         (inverse (make-array '(4 4)
                              :initial-contents
                              '((-5 2 6 -8)
                                (1 -5 1 8)
                                (7 7 -6 -7)
                                (1 -3 7 4))))
         (make-array '(4 4)
                     :initial-contents
                     '(( 0.21805 0.45113 0.24060 -0.04511 )
                       ( -0.80827 -1.45677 -0.44361 0.52068 )
                       ( -0.07895 -0.22368 -0.05263 0.19737 )
                       ( -0.52256 -0.81391 -0.30075 0.30639 )))))))

(deftest translation
  (testing "translation"
    (ok (equalp
         (let ((transform (translation 5 -3 2))
               (p (point -3 4 5)))
           (mm transform p))
         (point 2 1 7))))
  (testing "no translation of vectors"
    (ok (equalp
         (let ((transform (translation 5 -3 2))
               (v (vectorr -3 4 5)))
           (mm transform v))
         (vectorr -3 4 5)))))

(deftest scaling
  (testing "scaling"
    (ok (equalp
         (let ((transform (scaling 2 3 4))
               (p (point -4 6 8)))
           (mm transform p))
         (point -8 18 32))))
  (testing "scaling of vectors"
    (ok (equalp
         (let ((transform (scaling 2 3 4))
               (v (vectorr -4 6 8)))
           (mm transform v))
         (vectorr -8 18 32))))
  (testing "inverse of scaling matrix"
    (ok (equalp
         (let* ((transform (scaling 2 3 4))
                (inv (inverse transform))
                (v (vectorr -4 6 8)))
           (mm inv v))
         (vectorr -2 2 2)))))

(deftest rotation
  (testing "rotation-x half-quarter"
    (ok (approximately
         (let ((half-quarter (rotation-x (/ pi 4)))
               (p (point 0 1 0)))
           (mm half-quarter p))
         (point 0 (/ (sqrt 2) 2) (/ (sqrt 2) 2)))))
  (testing "rotation-x full-quarter"
    (ok (approximately
         (let ((full-quarter (rotation-x (/ pi 2)))
               (p (point 0 1 0)))
           (mm full-quarter p))
         (point 0 0 1))))
  (testing "around y-axis"
    (ok (approximately
         (let ((full-quarter (rotation-y (/ pi 2)))
               (p (point 0 0 1 )))
           (mm full-quarter p))
         (point 1 0 0))))
  (testing "around z-axis"
    (ok (approximately
         (let ((full-quarter (rotation-z (/ pi 2)))
               (p (point 0 1 0 )))
           (mm full-quarter p))
         (point -1 0 0)))))


(deftest shearing
  (testing "shearing, moves x in proportion to z"
    (testing "shearing, moves x in proportion to y"
      (ok (approximately
           (let ((transform (shearing 1 0 0 0 0 0))
                 (p (point 2 3 4)))
             (mm transform p))
           (point 5 3 4))))
    (testing "shearing, moves x in proportion to z"
      (ok (approximately
           (let ((transform (shearing 0 1 0 0 0 0))
                 (p (point 2 3 4)))
             (mm transform p))
           (point 6 3 4))))
    (testing "shearing, moves y in proportion to x"
      (ok (approximately
           (let ((transform (shearing 0 0 1 0 0 0))
                 (p (point 2 3 4)))
             (mm transform p))
           (point 2 5 4))))
    (testing "shearing, moves y in proportion to z"
      (ok (approximately
           (let ((transform (shearing 0 0 0 1 0 0))
                 (p (point 2 3 4)))
             (mm transform p))
           (point 2 7 4))))
    (testing "shearing, moves z in proportion to x"
      (ok (approximately
           (let ((transform (shearing 0 0 0 0 1 0))
                 (p (point 2 3 4)))
             (mm transform p))
           (point 2 3 6))))
    (testing "shearing, moves z in proportion to y"
      (ok (approximately
           (let ((transform (shearing 0 0 0 0 0 1))
                 (p (point 2 3 4)))
             (mm transform p))
           (point 2 3 7))))))

(deftest combining
  (testing "combining transformation matrices"
    (ok (equalp
         (let ((p (point 1 0 1))
               (a (rotation-x (/ pi 2)))
               (b (scaling 5 5 5))
               (c (translation 10 5 7)))
           (mm c (mm b (mm a p))))
         (point 15 0 7)))))
