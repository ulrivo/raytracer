(in-package :raytracer)

;; define a render function render-name
(defmacro defrender (render-name
                     file-name
                     camera-view-transform
                     world-light
                     world-shapes
                     &body body)
  `(defun ,render-name (width height)
     (let* (,@ body
               (camera (create-camera width height
                          (/ pi 3) ,camera-view-transform))
               (world (make-world :light ,world-light
                                  :shapes (list ,@world-shapes)))
               (canvas (render camera world)))
       (save-canvas canvas ,file-name))))

;; example call of defrender
;;
;; (defrender draw "draw.png"
;;     (view-transform (point 0 3 -10)
;;                     (point 0 1 0)
;;                     (vectorr 0 1 0))
;;     (make-light
;;      :position (point -1 15 -20)
;;      :intensity (color 1 1 1))
;;   (floor right)
;; (checkers (make-material
;;            :colour (color 1 0.1 0.1)
;;            :specular 0
;;            :reflective 0.1
;;            :pattern (make-checkers-pattern +black+ +white+)))
;; (floor (make-plane +identity-matrix+ checkers))
;; (right (make-plane
;;         (m* (translation 0 0 10)
;;             (rotation-y (/ pi 4))
;;             (rotation-x (/ pi 2)))
;;         checkers)))
