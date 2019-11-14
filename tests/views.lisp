(in-package :raytracer/tests/rays)

(deftest view-transformation-matrix
  (testing "transformation matrix for default orientation"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 -1))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  *identity-matrix*))))

  (testing "transformation matrix looking in positive z direction"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 1))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  (scaling -1 1 -1)))))

  (testing "transformation matrix moves the world"
    (ok (let* ((from (point 0 0 8))
               (to (point 0 0 0))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  (translation 0 0 -8)))))

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

(deftest camera-pixel
  (testing "pixel size for a horizontal canvas"
    (ok (approx (camera-pixel-size (create-camera 200 125 (/ pi 2)))
                       0.01)))

  (testing "pixel size for a vertical canvas"
    (ok (approx (camera-pixel-size
                 (create-camera 125 200 (/ pi 2)))
                0.01))))

(deftest camera-ray
  (testing "a ray thru the center of the canvas"
    (let* ((c (create-camera 201 101 (/ pi 2)))
           (r (ray-for-pixel c 100 50)))
      (ok (equalp (ray-origin r) (point 0 0 0)))
      (ok (approximately (ray-direction r) (vectorr 0 0 -1)))))

  (testing "a ray thru the corner of the canvas"
    (let* ((c (create-camera 201 101 (/ pi 2)))
           (r (ray-for-pixel c 0 0)))
      (ok (equalp (ray-origin r) (point 0 0 0)))
      (ok (approximately (ray-direction r)
                         (vectorr 0.66519 0.33259 -0.66851)))))

  (testing "a ray when the camera is transformed"
    (let ((c (create-camera 201 101 (/ pi 2))))
      (setf (camera-transform c)
            (mm (rotation-y (/ pi 4))
                (translation 0 -2 5)))
      (let ((r (ray-for-pixel c 100 50)))
        (ok (equalp (ray-origin r) (point 0 2 -5)))
        (ok (approximately (ray-direction r)
                           (vectorr (/ (sqrt 2) 2) 0
                                    (- (/ (sqrt 2) 2)))))))))

