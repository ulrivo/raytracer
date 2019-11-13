(in-package :raytracer/tests/rays)

(deftest view-default
  (testing "transformation matrix for default orientation"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 -1))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  *identity-matrix*)))))

(deftest view-positive
  (testing "transformation matrix looking in positive z direction"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 1))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  (scaling -1 1 -1))))))

(deftest view-moves
  (testing "transformation matrix moves the world"
    (ok (let* ((from (point 0 0 8))
               (to (point 0 0 0))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  (translation 0 0 -8))))))

(deftest view-arbitrary
  (testing "an arbitrary transformation matrix"
    (ok (let* ((from (point 1 3 2))
               (to (point 4 -2 8))
               (up (vectorr 1 1 0)))
          (approximately (view-transform from to up)
                  (make-array '(4 4)
                              :initial-contents
                              '((-0.50709  0.50709   0.67612  -2.36643)
                                (0.76772  0.60609   0.12122  -2.82843)
                                (-0.35857  0.59761  -0.71714   0.00000)
                                (0 0 0 1))))))))
