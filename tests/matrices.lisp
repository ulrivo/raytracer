(in-package :raytracer/tests/matrices)

;; NOTE: To run this test file, execute `(asdf:test-system :raytracer)'
;; in your Lisp.

(deftest matrix-multiplication
  (testing "correct matrix multiplication"
    (ok (equalp (m*
                 (mat 1 2 3 4 5 6 7 8 9 8 7 6 5 4 3 2)
                 (mat -2 1 2 3 3 2 1 -1 4 3 6 5 1 2 7 8))
                (mat 20 22 50 48 44 54 114 108 40 58 110 102 16 26 46 42))))
  (testing "correct matrix and vector multiplication"
    (ok (equalp (m*
                 (mat 1 2 3 4 2 4 4 2 8 6 4 1 0 0 0 1)
                 (point 1 2 3))
                (point 18 24 33)))))

(deftest mdet
  (testing "mdet of 2x2-matrix"
    (ok (= 17
             (mdet
              (mat 1 5 -3 2)))))
  (testing "mdet of 3x3-matrix"
    (ok (= -196
             (mdet
              (mat 1 2 6 -5 8 -4 2 6 4)))))
  (testing "mdet of 4x4-matrix"
    (ok (= -4071
             (mdet
              (mat -2 -8 3 5 -3 1 7 3 1 2 -9 6 -6 7 7 -9))))))

(deftest inverse
  (testing "error when mdet is zero"
    (ok (signals
            (minv (mat 0 0 0 2 -1 -7 6 -1 5)))))
  (testing "minv of a 4x4-matrix"
    (ok (approximately
         (minv (mat -5 2 6 -8 1 -5 1 8 7 7 -6 -7 1 -3 7 4))
         (mat 0.21805 0.45113 0.24060 -0.04511
               -0.80827 -1.45677 -0.44361 0.52068
               -0.07895 -0.22368 -0.05263 0.19737
               -0.52256 -0.81391 -0.30075 0.30639 )))))

(deftest translation
  (testing "translation"
    (ok (equalp
         (let ((transform (mtranslation (vec 5 -3 2)))
               (p (point -3 4 5)))
           (m* transform p))
         (point 2 1 7))))
  (testing "no translation of vectors"
    (ok (equalp
         (let ((transform (mtranslation (vec 5 -3 2)))
               (v (vectorr -3 4 5)))
           (m* transform v))
         (vectorr -3 4 5)))))

(deftest scaling
  (testing "scaling"
    (ok (equalp
         (let ((transform (mscaling (vec 2 3 4)))
               (p (point -4 6 8)))
           (m* transform p))
         (point -8 18 32))))
  (testing "scaling of vectors"
    (ok (equalp
         (let ((transform (mscaling (vec 2 3 4)))
               (v (vectorr -4 6 8)))
           (m* transform v))
         (vectorr -8 18 32))))
  (testing "minv of scaling matrix"
    (ok (equalp
         (let* ((transform (mscaling (vec 2 3 4)))
                (inv (minv transform))
                (v (vectorr -4 6 8)))
           (m* inv v))
         (vectorr -2 2 2)))))
(deftest rotation
  (testing "rotation-x half-quarter"
    (ok (approximately
         (let ((half-quarter (mrotation (vec 1 0 0) (/ pi 4)))
               (p (point 0 1 0)))
           (m* half-quarter p))
         (point 0 (/ (sqrt 2) 2) (/ (sqrt 2) 2)))))
  (testing "rotation-x full-quarter"
    (ok (approximately
         (let ((full-quarter (mrotation (vec 1 0 0) (/ pi 2)))
               (p (point 0 1 0)))
           (m* full-quarter p))
         (point 0 0 1))))
  (testing "around y-axis"
    (ok (approximately
         (let ((full-quarter (mrotation (vec 0 1 0) (/ pi 2)))
               (p (point 0 0 1 )))
           (m* full-quarter p))
         (point 1 0 0))))
  (testing "around z-axis"
    (ok (approximately
         (let ((full-quarter (mrotation (vec 0 0 1) (/ pi 2)))
               (p (point 0 1 0 )))
           (m* full-quarter p))
         (point -1 0 0)))))

(deftest combining
  (testing "combining transformation matrices"
    (ok (equalp
         (let ((p (point 1 0 1))
               (a (mrotation (vec 1 0 0) (/ pi 2)))
               (b (mscaling (vec 5 5 5)))
               (c (mtranslation (vec 10 5 7))))
           (m* c (m* b (m* a p))))
         (point 15 0 7)))))
