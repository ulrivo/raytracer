(in-package :raytracer)

(defvar +epsilon+ 0.0001)

(defvar +white+ (vec 1. 1. 1. ))
(defvar +black+ (vec 0. 0. 0. ))
(defvar +red+ (vec 1. 0. 0. ))
(defvar +green+ (vec 0. 1. 0. ))
(defvar +blue+ (vec 0. 0. 1. ))
(defvar +yellow+ (vec 1. 1. 0. ))
(defvar +lime+ (vec 0.749 1. 0. ))
(defvar +pink+ (vec 0.957 0.76 0.76 ))
(defvar +orange+ (vec 1. 0.5 0. ))

(defvar +identity-matrix+ (meye 4))
