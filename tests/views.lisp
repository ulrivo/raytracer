(in-package :raytracer/tests/rays)

(deftest view-transformation-matrix
  (testing "transformation matrix for default orientation"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 -1))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  +identity-matrix+))))

  (testing "transformation matrix looking in positive z direction"
    (ok (let* ((from (point 0 0 0))
               (to (point 0 0 1))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  (mscaling (vec -1 1 -1))))))

  (testing "transformation matrix moves the world"
    (ok (let* ((from (point 0 0 8))
               (to (point 0 0 0))
               (up (vectorr 0 1 0)))
          (equalp (view-transform from to up)
                  (mtranslation (vec 0 0 -8))))))

  (testing "an arbitrary transformation matrix"
    (ok (let* ((from (point 1 3 2))
               (to (point 4 -2 8))
               (up (vectorr 1 1 0)))
          (approximately (view-transform from to up)
              (MAT
               -0.50709 0.50709 0.67612 -2.36643
               0.76772 0.60609 0.12122 -2.82843
               -0.35857 0.59761 -0.71714 0.0
               0.0 0.0 0.0 1.0))))))

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
                      (view-transform from to up))))
          (approximately (pixel-at (render c w) 5 5)
                         (color 0.38066 0.47583 0.2855))))))

;; is-shadowed
(deftest is-shadowed
    (let ((w (default-world)))
      (testing "there is no shadow XXX when nothing is collinear with point and light"
               (ng (is-shadowed w (point 0 10 0))))
      (testing "The shadow when an object is between the point and the light"
               (ok (is-shadowed w (point 10 -10 10))))
      (testing "there is no shadow when an object is behind the light"
               (ng (is-shadowed w (point -20 20 -20))))
      (testing "there is no shadow when an object is behind the point"
               (ng (is-shadowed w (point -2 2 -2))))))

;; reflection
(deftest reflection
  (testing "precomputing the refleciton vector"
    (ok (let* ((p (default-plane))
           (sqr (/ (sqrt 2) 2))
           (r (make-ray :origin (point 0 1 -1)
                       :direction (vectorr 0 (- sqr) sqr)))
            (i (make-intersektion :tt (sqrt 2)
                                  :object p))
           (comps (prepare-computations i r)))
      (approximately
             (computations-reflectv comps)
             (vectorr 0 sqr sqr))))))

(deftest reflected-color
  (testing "the reflected color for a nonreflective material"
    (ok (let* ((w (default-world))
               (r (make-ray :origin (point 0 0 0)
                            :direction (vectorr 0 0 1)))
               (shape (second (world-shapes w))))
          (setf (material-ambient (shape-material shape)) 1)
          (let* ((i (make-intersektion :tt 1 :object shape))
                 (comps (prepare-computations i r))
                 (color (reflected-color w comps 4)))
            (equalp color +black+)))))
  (testing "the reflected color for a reflective material"
    (ok (let* ((w (default-world))
               (sqr (/ (sqrt 2) 2))
               (r (make-ray :origin (point 0 0 -3)
                            :direction (vectorr 0 (- sqr) sqr )))
               (shape (make-plane
                       (translation 0 -1 0)
                       (make-material :reflective 0.5))))
          (setf (world-shapes w) (cons shape (world-shapes w)))
          (let* ((i (make-intersektion :tt (sqrt 2) :object shape))
                 (comps (prepare-computations i r))
                 (color (reflected-color w comps 4)))
            (approximately color
                           (color 0.19032 0.2379 0.14274)))))))

(deftest mutually-reflective
  (testing "color-at with mutually reflective surfaces"
    (let* ((w (make-world))
           (lower (make-plane
                   (translation 0 -1 0)
                   (make-material
                    :reflective 1)))
           (upper (make-plane
                   (translation 0 1 0)
                   (make-material
                    :reflective 1)))
           (r (make-ray :origin (point 0 0 0)
                        :direction (vectorr 0 1 0))))
      (setf (world-light w) (make-light
                             :position (point 0 0 0)
                             :intensity (color 1 1 1)))
      (setf (world-shapes w) (list lower upper))
      (ng (equalp +black+ (color-at w r))))))
