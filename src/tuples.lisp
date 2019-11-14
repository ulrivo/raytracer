(in-package :raytracer)

(defconstant +white+ (vector 1. 1. 1. ))
(defconstant +black+ (vector 0. 0. 0. ))
(defconstant +red+ (vector 1. 0. 0. ))
(defconstant +green+ (vector 0. 1. 0. ))
(defconstant +blue+ (vector 0. 0. 1. ))
(defconstant +yellow+ (vector 1. 1. 0. ))
(defconstant +lime+ (vector 0.749 1. 0. ))
(defconstant +pink+ (vector 0.957 0.76 0.76 ))
(defconstant +orange+ (vector 1. 0.5 0. ))

;; point, vectorr

(defun point (x y z)
  (make-array 4 :initial-contents (list x y z 1)))

(defun vectorr (x y z)
  (make-array 4 :initial-contents (list x y z 0)))

;; tuple additon and subtraciton
(defun tadd (v w)
  (map 'vector #'+ v w))

(defun tsub (v w)
  (map 'vector #'- v w))

(defun tnegate (v)
  (tsub (vectorr 0 0 0) v))

(defconstant +epsilon+ 0.00001)

(defun 2d-array-to-list (array)
  (apply #'append 
         (loop for i below (array-dimension array 0)
               collect (loop for j below (array-dimension array 1)
                             collect (aref array i j)))))

(defun approximately (v w)
  (flet ((simple-arrayp (a)
           (eql 'simple-array (car (type-of a)))))
    (let ((r v)
          (s w))
      (when (simple-arrayp v)
        (setf r (2d-array-to-list v)))
      (when (simple-arrayp w)
        (setf s (2d-array-to-list w)))
      (loop for element in
                        (map 'list (lambda (x)
                                     (< (abs x) +epsilon+))
                             (tsub r s)) always element))))

(defun approx (x y)
  (< (abs (- x y)) +epsilon+))

;; scalar multiplication division

(defun mults (v s)
  (map 'vector (lambda (c)(* c s)) v))

(defun divs (v s)
  (map 'vector (lambda (c)(/ c s)) v))

;; magnitude

(defun magnitude (v)
  (sqrt (reduce #'+ (map 'list #'* v v))))

(defun normalize (v)
  (divs v (magnitude v)))

;; dot product

(defun dot (v w)
  (reduce #'+ (map 'list #'* v w)))

;; cross product

(defun cross (a b)
  (vectorr
   (- (* (elt a 1) (elt b 2)) (* (elt a 2) (elt b 1)))
   (- (* (elt a 2) (elt b 0)) (* (elt a 0) (elt b 2)))
   (- (* (elt a 0) (elt b 1)) (* (elt a 1) (elt b 0)))))

;; color

(defun color (x y z)
  (vector x y z))

(defun hadamard-product (c d) (map 'vector #'* c d))

;; canvas

(defun canvas (width height)
  (make-array (list width height)
              :initial-element (vector 0 0 0)))

(defun write-pixel (canvas x y color)
  (let* ((dimension (array-dimensions canvas))
         (w (first dimension))
         (h (second dimension))
         (xi (truncate x))
         (yi (truncate y)))
    (and (<= 0 xi) (< xi w) (<= 0 yi) (< yi h)
         (setf (aref canvas xi yi) color))))

;; given a string, if it is longer than 70 characters,
;; find the last space before 70 character and replace it with a newline,
;; repeat this on rest
;; answer a string

(defun last-space (string)
  (position #\ (subseq string 0 70) :from-end t))

(defun no-longer (string)
  (if (< 70 (length string))
      (let ((pos (last-space string)))
        (format nil "~A~%~A"
                (subseq string 0 pos)
                (no-longer (subseq string (1+ pos)))))
      string))

;; (defun canvas-to-ppm (canvas)
;;   (let* ((dimension (array-dimensions canvas))
;;          (w (first dimension))
;;          (h (second dimension))
;;          (data (format nil "P3~%~A ~A~%255~%" w h)))
;;     (flet ((concat (s)
;;              (setf data
;;                    (concatenate
;;                     'string
;;                     data s))))
;;       (dotimes (j h)
;;         (let ((line nil))
;;           (dotimes (i w)
;;             (let ((col (aref canvas i j)))
;;               (dotimes (k 3)
;;                 (push (min 255 (truncate (* 255 (elt col k)))) line))))
;;           (concat (no-longer (format nil "~{~A ~}~%" (reverse line)))))))
;;     data))

;; (defun save-canvas (canvas filename)
;;   "save a canvas to a file"
;;   (with-open-file (str filename
;;                        :direction :output
;;                        :if-exists :supersede
;;                        :if-does-not-exist :create)
;;     (format str "~A" (canvas-to-ppm canvas))))

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
