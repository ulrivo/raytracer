(in-package :raytracer)

;; point, vectorr

(defun point (x y z)
  (make-array 4 :initial-contents (list x y z 1)))

(defun vectorr (x y z)
  (make-array 4 :initial-contents (list x y z 0)))

;; tuple additon and subtraciton
(defun tadd (x y)
  (map 'vector #'+ x y))

(defun tsub (x y)
  (map 'vector #'- x y))

(defun tnegate (x)
  (tsub (vector 0 0 0) x))

(defconstant +epsilon+ 0.00001)

(defun approximately (v w)
  (loop for element in (map 'list (lambda (x)
                    (< (abs x) +epsilon+))
            (tsub v w)) always element))

;; scalar multiplication division

(defun mults (x s)
  (map 'vector (lambda (c)(* c s)) x))

(defun divs (x s)
  (map 'vector (lambda (c)(/ c s)) x))

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

(defun canvas (w h)
  (make-array (list w h) :initial-element (vector 0 0 0)))

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

(defun canvas-to-ppm (canvas)
  (let* ((dimension (array-dimensions canvas))
         (w (first dimension))
         (h (second dimension))
         (data (format nil "P3~%~A ~A~%255~%" w h)))
    (flet ((concat (s)
             (setf data
                   (concatenate
                    'string
                    data s))))
      (dotimes (i w)
        (let ((line nil))
          (dotimes (j h)
            (let ((col (aref canvas i j)))
              (dotimes (k 3)
                (push (truncate (* 255 (elt col k))) line))))
          (concat (no-longer (format nil "~{~A ~}~%" (reverse line)))))))
    data))

(defun save-canvas (canvas filename)
  (with-open-file (str filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format str "~A" (canvas-to-ppm canvas))))
