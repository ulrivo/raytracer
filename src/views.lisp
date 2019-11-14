(in-package :raytracer)

(defun view-transform (from to up)
  (let* ((forward (normalize (tsub to from)))
         (upn (normalize up))
         (left (cross forward upn))
         (true-up (cross left forward))
         (nforward (mults forward -1))
         (nfrom (mults from -1))
         (orientation (make-array '(4 4)
            :initial-contents
            (list
             (list (aref left 0)
                   (aref left 1)
                   (aref left 2) 0)
             (list (aref true-up 0)
                   (aref true-up 1)
                   (aref true-up 2) 0)
             (list (aref nforward 0)
                   (aref nforward 1)
                   (aref nforward 2) 0)
              (list 0 0 0 1)))))
    (mm orientation (translation (aref nfrom 0)
                                 (aref nfrom 1)
                                 (aref nfrom 2)))))


(defstruct camera
  hsize vsize field-of-view transform
  pixel-size half-width half-height)

(defun create-camera (hsize vsize field-of-view
                      &optional (transform *identity-matrix*))
  (let* ((half-view (tan (/ field-of-view 2)))
         (aspect (/ hsize vsize))
         (half-width half-view)
         (half-height half-view)
         (pixel-size))
    (if (<= 1 aspect)
        (setf half-height (/ half-view aspect))
        (setf half-width  (* half-view aspect)))
    (setf pixel-size (/ (* half-width 2) hsize))
    (make-camera
     :hsize hsize
     :vsize vsize
     :field-of-view field-of-view
     :transform transform
     :pixel-size pixel-size
     :half-width half-width
     :half-height half-height)))

(defun ray-for-pixel (camera px py)
  (let* ((xoffset (* (camera-pixel-size camera) (+ px 0.5)))
         (yoffset (* (camera-pixel-size camera) (+ py 0.5)))
         (world-x (- (camera-half-width camera) xoffset))
         (world-y (- (camera-half-height camera) yoffset))
         (pixel (mm (inverse (camera-transform camera))
                    (point world-x world-y -1)))
         (origin (mm (inverse (camera-transform camera))
                     (point 0 0 0)))
         (direction (normalize (tsub pixel origin))))
    (make-ray :origin origin :direction direction)))

(defun render (camera world)
  (let ((image (canvas (camera-hsize camera) (camera-vsize camera))))
    (dotimes (y (camera-vsize camera))
      (dotimes (x (camera-hsize camera))
        (let*  ((ray (ray-for-pixel camera x y))
                (color (color-at world ray)))
          (write-pixel image x y color))))
    image))

(defun is-shadowed (world point)
  (let* ((v (tsub (light-position (world-light world)) point))
         (distance (magnitude v))
         (direction (normalize v))
         (r (make-ray :origin point :direction direction))
         (is (intersect-world world r))
         (h (hit is)))
    (and h (< (intersektion-tt h) distance))))
