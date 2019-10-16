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
