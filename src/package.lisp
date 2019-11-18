(defpackage raytracer
  (:use #:cl)
  (:export
     #:approx
     #:approximately
     #:camera-pixel-size
     #:camera-transform
     #:cofactor
     #:color
     #:color-at
     #:computations-eyev
     #:computations-inside
     #:computations-normalv
     #:computations-object
     #:computations-over-point 
     #:computations-point
     #:computations-tt
     #:create-camera
     #:default-plane
     #:default-sphere
     #:default-world
     #:determinant
     #:draw
     #:+epsilon+
     #:hit
     #:+identity-matrix+
     #:intersect
     #:intersect-world
     #:intersektion-object
     #:intersektion-tt
     #:inverse
     #:is-shadowed
     #:light-intensity
     #:light-position
     #:lighting
     #:make-intersektion
     #:make-light
     #:make-material
     #:make-plane
     #:make-ray
     #:make-sphere
     #:make-world
     #:material-ambient
     #:material-colour
     #:material-diffuse
     #:material-shininess
     #:material-specular
     #:minor
     #:mm
     #:normal-at
     #:pixel-at
     #:point
     #:prepare-computations
     #:ray-direction
     #:ray-for-pixel
     #:ray-origin
     #:ray-position
     #:reflect
     #:render
     #:rotation-x
     #:rotation-y
     #:rotation-z
     #:save-canvas
     #:scaling
     #:shade-hit
     #:shearing
     #:shape-material 
     #:shape-transform
     #:sphere
     #:submatrix
     #:transform
     #:translation
     #:vectorr
     #:view-transform
     #:world-light
     #:world-shapes
     )
  )
