(defpackage raytracer
  (:use :cl))
(in-package :raytracer)

(defstruct projectile
  position velocity)

(defstruct environment
  gravity wind)

(defparameter *pro*
  (make-projectile :position (point 0 1 0)
                   :velocity (normalize (vectorr 1 1 0))))

(defparameter *env*
  (make-environment :gravity (vectorr 0 -0.1 0)
                    :wind (vectorr -0.01 0 0)))

(defun tick (en pro)
  (make-projectile
   :position (tadd (projectile-position pro) (projectile-velocity pro))
   :velocity (tadd (projectile-velocity pro)
                   (tadd (environment-wind en) (environment-gravity en)))))

(defun shot (w h)
  (let* ((c (canvas w h))
         (start (point 0 1 0))
         (velocity (mults (normalize (vectorr 1 1.8 0)) 11.25))
         (p (make-projectile :position start :velocity velocity))
         (gravity (vectorr 0 -0.1 0))
         (wind (vectorr -0.01 0 0))
         (e (make-environment :gravity gravity :wind wind)))
    (loop while (< 0 (elt (projectile-position p) 1))
          do
             (setf p (tick e p))
             (write-pixel c
                          (elt (projectile-position p) 0)
                          (elt (projectile-position p) 1)
                          (color 1 0 0)))
             ;; (format t "x ~A, y ~A~%"
             ;;         (elt (projectile-position p) 0)
             ;;         (elt (projectile-position p) 1)))
    (save-canvas c "test.ppm")))

(defun test ()
  (let ((c (canvas 4 4)))
    (write-pixel c 0 1.9 (color 1 0 0))
    (write-pixel c 0 2.6 (color 1 0 0))
    c))
