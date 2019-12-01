(in-package :raytracer/tests/rays)

(deftest view-transformation-matrix
  (testing "transformation matrix for default orientation"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 -1))
               (up (vectorr 0 1 0)))
          (equalp (mlookat from to up)
                  +identity-matrix+))))

  (testing "transformation matrix looking in positive z direction"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 1))
               (up (vectorr 0 1 0)))
          (equalp (mlookat from to up)
                  (mscaling (vec -1 1 -1))))))

  (testing "transformation matrix moves the world"
    (ok (let* ((from (point 0 0 8))
               (to (point 0 0 0))
               (up (vectorr 0 1 0)))
          (equalp (mlookat from to up)
                  (mtranslation (vec 0 0 -8))))))

  (testing "an arbitrary transformation matrix"
    (ok (let* ((from (point 1 3 2))
               (to (point 4 -2 8))
               (up (vectorr 1 1 0)))
          (approximately (mlookat from to up)
              (MAT
                -0.51449573 0.51449573 0.6859944 -2.4009802 0.7789241 0.61494005 0.122988015
                -2.8697202 -0.35856858 0.59761435 -0.71713716 -2.3841858e-7 0.0 0.0 0.0 1.0))))))
(deftest camera-pixel
  (testing "pixel size for a horizontal canvas"
    (ok (approximately (camera-pixel-size (create-camera 200 125 (/ pi 2)))
                       0.01)))

  (testing "pixel size for a vertical canvas"
    (ok (approximately (camera-pixel-size
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
    (let* ((c (create-camera 201 101 (/ pi 2)
                             (m* (mrotation (vec 0 1 0) (/ pi 4)) (mtranslation (vec 0 -2 5)))))
          (r (ray-for-pixel c 100 50)))
      (ok (approximately (ray-origin r) (point 0 2 -5)))
      (ok (approximately (ray-direction r)
            (vectorr (/ (sqrt 2) 2) 0 (- (/ (sqrt 2) 2))))))))

(deftest render
  (testing "rendering a world with a camera"
    (ok (let* ((w (default-world))
               (from (point 0 0 -5))
               (to (point 0 0 0))
               (up (vectorr 0 1 0))
               (c (create-camera 11 11 (/ pi 2)
                      (mlookat from to up))))
          (approximately (pixel-at (render c w) 5 5)
                         (color 0.38066 0.47583 0.2855))))))

(deftest is-shadowed
    (let ((w (default-world)))
      (testing "there is no shadow when nothing is collinear with point and light"
               (ng (is-shadowed w (point 0 10 0))))
      (testing "The shadow when an object is between the point and the light"
               (ok (is-shadowed w (point 10 -10 10))))
      (testing "there is no shadow when an object is behind the light"
               (ng (is-shadowed w (point -20 20 -20))))
      (testing "there is no shadow when an object is behind the point"
               (ng (is-shadowed w (point -2 2 -2))))))
