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
     #:make-intersektion
     #:intersektion-tt
     #:intersektion-object
     #:ray-position
     #:intersect
     )
  (:use #:cl))

