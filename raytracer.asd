(defsystem "raytracer"
  :version "0.1.0"
  :author "Ulrich Vollert <lisp@ulrivo.de>"
  :license ""
  :depends-on ("png")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "tuples")
                 (:file "matrices")
                 (:file "shapes")
                 (:file "rays")
                 (:file "views")
                 (:file "main"))))
  :description "Raytracer by Jamis Buck"
  :in-order-to ((test-op (test-op "raytracer/tests"))))

(defsystem "raytracer/tests"
  :author "Ulrich Vollert <lisp@ulrivo.de>"
  :license ""
  :depends-on ("raytracer"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "matrices")
                 (:file "rays")
                 (:file "views")
                 (:file "main"))))
  :description "Test system for raytracer"
  :perform (test-op (op c) (symbol-call :rove :run c)))
