(in-package :raytracer)

(defun approximately (m1 m2)
  (typecase m1
    (mat (< (m2norm (m- m1 m2)) (* +epsilon+ 16)))
    (vec (< (v2norm (v- m1 m2)) (* +epsilon+ 4)))
    (otherwise (< (abs (- m1 m2)) +epsilon+))))

(defun translation (x y z)
  (mtranslation (vec x y z)))

(defun scaling (x y z)
  (mscaling (vec x y z)))

(defun rotation-x (p)
  (mrotation (vec 1 0 0) p))

(defun rotation-y (p)
  (mrotation (vec 0 1 0) p))

(defun rotation-z (p)
  (mrotation (vec 0 0 1) p))

(defun canvas (width height)
  (make-array (list width height)))

(defun write-pixel (canvas x y color)
  (setf (aref canvas x y) color))

(defun pixel-at (canvas x y)
  (aref canvas x y))

(defun save-canvas (canvas filename)
  "save a canvas to a file"
  (let* ((dimension (array-dimensions canvas))
         (w (first dimension))
         (h (second dimension))
         (new (png:make-image h w 3)))
    (dotimes (j h)
      (dotimes (i w)
        (let ((col (vclamp 0 (v* (aref canvas i j) 256) 255)))
          (setf (aref new j i 0) (truncate (vx col)))
          (setf (aref new j i 1) (truncate (vy col)))
          (setf (aref new j i 2) (truncate (vz col))))))
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

