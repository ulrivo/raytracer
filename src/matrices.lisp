(in-package :raytracer)

(defun mm (a b)
  (let* ((dim-a (array-dimensions a))
         (dim-b (array-dimensions b))
         (a1 (first dim-a))
         (a2 (second dim-a))
         (b1 (first dim-b))
         (b2 (second dim-b))
         )
    (when (/= a2 b1)
      (error "dimensions not compatible for multiplication"))
    (if b2
        (let ((result (make-array (list a1 b2))))
          (dotimes (i a1)
            (dotimes (j b2)
              (let ((s 0))
                (dotimes (k a2)
                  (incf s (* (aref a i k) (aref b k j))))
                (setf (aref result i j) s))))
          result)
        (let ((result (make-array (list a1))))
          (dotimes (i a1)
            (let ((s 0))
              (dotimes (k a2)
                (incf s (* (aref a i k) (aref b k))))
              (setf (aref result i) s)))
          result))))

(defconstant +identity-matrix+
  (make-array '(4 4)
              :initial-contents
              '((1 0 0 0)
                (0 1 0 0)
                (0 0 1 0)
                (0 0 0 1))))

(defun transpose (m)
  (let* ((dim (array-dimensions m))
         (d1 (first dim))
         (d2 (second dim)))
    (when (/= 2 (length dim))
      (error "transpose only for 2-dimensional matrices"))
    (let ((result (make-array (list d2 d1))))
      (dotimes (i d1)
        (dotimes (j d2)
          (setf (aref result j i) (aref m i j))))
      result)))

(defun determinant (m)
  (let* ((dim (array-dimensions m))
         (d1 (first dim))
         (d2 (second dim)))
    (when (or (/= 2 (length dim))
              (/= d1 d2))
      (error "not quadratic or not 2-dimensional"))
    (if (= 2 d1)
        (- (* (aref m 0 0) (aref m 1 1))
           (* (aref m 1 0) (aref m 0 1)))
        (let ((result 0))
          (dotimes (c d1)
              (incf result (* (aref m 0 c)
                              (cofactor m 0 c))))
          result))))

(defun submatrix (m r c)
  (let* ((dim (array-dimensions m))
         (d1 (1- (first dim)))
         (d2 (1- (second dim)))
         (result (make-array (list d1 d2))))
    (dotimes (i d1)
      (dotimes (j d2)
        (setf (aref result i j)
              (let ((i1 (if (<= r i) (1+ i) i))
                    (j1 (if (<= c j) (1+ j) j)))
                (aref m i1 j1)))))
    result))

(defun minor (m r c)
  (determinant (submatrix m r c)))

(defun cofactor (m r c)
  (let ((sign (if (oddp (+ r c)) -1 1)))
    (* sign (minor m r c))))

(defun inverse (m)
  (let* ((dim (array-dimensions m))
         (d1 (first dim))
         (det (determinant m)))
    (when (zerop det)
      (error "determinant is zero, no inverse"))
    (let ((result (make-array (array-dimensions m))))
      (dotimes (r d1)
        (dotimes (c d1)
          (setf (aref result c r) (/ (cofactor m r c) det))))
      result)))

(defun translation (x y z)
  (make-array '(4 4)
              :initial-contents
              (list
               (list 1 0 0 x)
               (list 0 1 0 y)
               (list 0 0 1 z)
               (list 0 0 0 1))))

(defun scaling (x y z)
  (make-array '(4 4)
              :initial-contents
              (list
               (list x 0 0 0)
               (list 0 y 0 0)
               (list 0 0 z 0)
               (list 0 0 0 1))))

(defun rotation-x (r)
  (make-array '(4 4)
              :initial-contents
              (list
               (list 1 0 0 0)
               (list 0 (cos r) (- (sin r)) 0)
               (list 0 (sin r) (cos r) 0)
               (list 0 0 0 1))))

(defun rotation-y (r)
  (make-array '(4 4)
              :initial-contents
              (list
               (list (cos r) 0 (sin r) 0)
               (list 0 1 0 0)
               (list (- (sin r)) 0 (cos r) 0)
               (list 0 0 0 1))))

(defun rotation-z (r)
  (make-array '(4 4)
              :initial-contents
              (list
               (list (cos r) (- (sin r)) 0 0)
               (list (sin r) (cos r) 0 0 )
               (list 0 0 1 0)
               (list 0 0 0 1))))

(defun shearing (xy xz yx yz zx zy)
  (make-array '(4 4)
              :initial-contents
              (list
               (list 1 xy xz 0)
               (list yx 1 yz 0)
               (list zx zy 1 0)
               (list 0 0 0 1)))) 


