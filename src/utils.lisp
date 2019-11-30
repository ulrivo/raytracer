(in-package :raytracer)

(defun approximately (m1 m2)
  (typecase m1
    (mat (< (m2norm (m- m1 m2)) (* +epsilon+ 16)))
    (vec (< (v2norm (v- m1 m2)) (* +epsilon+ 4)))
    (otherwise (< (abs (- m1 m2)) +epsilon+))))

(defun save-canvas (canvas filename)
  "save a canvas to a file"
  (let* ((dimension (array-dimensions canvas))
         (w (first dimension))
         (h (second dimension))
         (new (png:make-image h w 3)))
    (dotimes (j h)
      (dotimes (i w)
        (let ((col (aref canvas i j)))
          (dotimes (k 3)
            (setf (aref new j i k) (min 255 (truncate (* 255 (elt col k)))))))))
    (with-open-file (output filename :element-type '(unsigned-byte 8)
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (png:encode new output))))


(defun ray-for-pixel (camera px py)
  (let* ((xoffset (* (camera-pixel-size camera) (+ px 0.5)))
         (yoffset (* (camera-pixel-size camera) (+ py 0.5)))
         (world-x (- (camera-half-width camera) xoffset))
         (world-y (- (camera-half-height camera) yoffset))
         (pixel (m* (minv (camera-transform camera))
                    (point world-x world-y -1)))
         (origin (m* (minv (camera-transform camera))
                     (point 0 0 0)))
         (direction (vunit (v- pixel origin))))
    (make-ray :origin origin :direction direction)))

(defun render (camera world)
  (let ((image (canvas (camera-hsize camera) (camera-vsize camera))))
    (dotimes (y (camera-vsize camera))
      (dotimes (x (camera-hsize camera))
        (let*  ((ray (ray-for-pixel camera x y))
                (color (color-at world ray)))
          (write-pixel image x y color))))
    image))

