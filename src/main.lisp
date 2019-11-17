(in-package :raytracer)

(defun draw (width height)
  (let* ((floor (make-sphere
                 :transform (scaling 10 0.01 10)
                 :material (make-material
                            :colour (color 1 0.9 0.9)
                            :specular 0)))
         (left-wall (make-sphere
                     :transform (mm (translation 0 0 5)
                                (mm (rotation-y (- (/ pi 4)))
                                (mm (rotation-x (/ pi 2))
                                     (scaling 10 0.01 10))))
                     :material (sphere-material floor)))
         (right-wall (make-sphere
                      :transform (mm (translation 0 0 5)
                                     (mm (rotation-y (/ pi 4))
                                         (mm (rotation-x (/ pi 2))
                                             (scaling 10 0.01 10))))
                      :material (sphere-material floor)))
         (middle (make-sphere
                  :transform (translation -0.5 1 0.5)
                  :material (make-material
                             :colour (color 0.1 1 0.5)
                             :diffuse 0.7
                             :specular 0.3)))
         (right (make-sphere
                 :transform (mm (translation 1.5 1 -0.5)
                                (scaling 0.5 0.5 0.5))
                 :material (make-material
                            :colour (color 0.5 1 0.1)
                            :diffuse 0.7
                            :specular 0.3)))
         (left (make-sphere
                 :transform (mm (translation -1.5 0.33 -0.75)
                                (scaling 0.33 0.33 0.33))
                     :material (make-material
                                :colour (color 1 0.8 0.1)
                                :diffuse 0.7
                                :specular 0.3)))
         (camera (create-camera  width height (/ pi 3)
                                (view-transform (point 0 1.5 -5)
                                                (point 0 1 0)
                                                (vectorr 0 1 0))))
         (world (make-world
                 :light (make-light
                         :position (point -10 10 -10)
                         :intensity (color 1 1 1))
                 :shapes (list floor left-wall right-wall
                          middle right left )))
         (canvas (render camera world)))
    (save-canvas canvas "sphere500.png")))
