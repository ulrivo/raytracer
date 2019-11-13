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
                                    (list (aref left 0) (aref left 1) (aref left 2) 0)
                                    (list (aref true-up 0) (aref true-up 1) (aref true-up 2) 0)
                                    (list (aref nforward 0) (aref nforward 1) (aref nforward 2) 0)
                                     (list 0 0 0 1)))))
    (mm orientation (translation (aref nfrom 0)
                                 (aref nfrom 1)
                                 (aref nfrom 2)))))
