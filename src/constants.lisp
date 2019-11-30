(in-package :raytracer)

(defconstant +epsilon+ 0.00001)

(defconstant +white+ (vec 1. 1. 1. ))
(defconstant +black+ (vec 0. 0. 0. ))
(defconstant +red+ (vec 1. 0. 0. ))
(defconstant +green+ (vec 0. 1. 0. ))
(defconstant +blue+ (vec 0. 0. 1. ))
(defconstant +yellow+ (vec 1. 1. 0. ))
(defconstant +lime+ (vec 0.749 1. 0. ))
(defconstant +pink+ (vec 0.957 0.76 0.76 ))
(defconstant +orange+ (vec 1. 0.5 0. ))

(defconstant +identity-matrix+ (meye 4))
