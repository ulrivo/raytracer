(defpackage #:raytracer
  (:export
     #:approximately
     #:point
     #:vectorr
     #:mm
     #:determinant
     #:submatrix
     #:minor
     #:cofactor
     #:inverse
     #:translation
     #:scaling
     #:rotation-x
     #:rotation-y
     #:rotation-z
     #:shearing
     #:make-ray
     #:make-sphere
     #:sphere-transform
     #:make-intersektion
     #:intersektion-tt
     #:intersektion-object
     #:ray-position
     #:ray-origin
     #:ray-direction
     #:intersect
     #:hit
     #:transform
     #:draw
     )
  (:use #:cl))

