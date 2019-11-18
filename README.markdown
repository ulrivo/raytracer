# Raytracer

## Project initialization

### New project created with cl-project

[cl-cookbook](https://lispcookbook.github.io/cl-cookbook/getting-started.html)

(ql:quickload "cl-project")
(cl-project:make-project #P"./path-to-project/root/")

Additional explaination

[Xach quick-project](https://xach.livejournal.com/278047.html)

#### Added link as in comment of last URL

“ASDF2's default setup also scans a directory called ~/.local/share/common-lisp/source/, 
so if you don't mind putting projects there, …”

You don’t have to: just make a symbolic link to your projects directory here.
ln -s ~/path/to/my/projects ~/.local/share/common-lisp/source/

ln -s ~/Dev/lisp/src/ ~/.local/share/common-lisp/source/

### Start development in Slime

(ql:quickload :raytracer)

,in-package raytracer

### Testing

To run this test file, execute `(asdf:test-system :raytracer)' in your Lisp.

Or, start stand-alone SBCL, load test-package

(ql:quickload :raytracer/tests)

and run tests with

(rove:run :raytracer/tests)


## Test
(asdf:operate 'asdf:test-op :raytracer)
## Usage

(ql:quickload :raytracer)

(load "main.lisp")

### Debugging

Turn off optimiztions to see all steps in (step (function....)):

(declaim (optimize (speed 0) (space 0) (debug 3)))

## Installation
 
