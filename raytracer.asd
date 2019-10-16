(defsystem "raytracer"
  :version "0.1.0"
  :author "Ulrich Vollert <lisp@ulrivo.de"
  :license ""
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components
                ((:file "tuples")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "raytracer/tests"))))

(defsystem "raytracer/tests"
  :author "Ulrich Vollert <lisp@ulrivo.de"
  :license ""
  :depends-on ("raytracer"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for raytracer"
  :perform (test-op (op c) (symbol-call :rove :run c)))
