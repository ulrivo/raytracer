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

- Taken from [Debugging Lisp](https://malisper.me/category/debugging-common-lisp/) 

#### Turn on debugging

Turn off optimiztions to see all steps in (step (function....)):

`(declaim (optimize (speed 0) (space 0) (debug 3)))`

#### Recompilation and break

- Insert dynamically `(break)`

- Point to a frame and press 'r' to restart on frame

#### Inspecting

- Right click on object like `#<POINT {10031070F3}>`, or C-c C-v TAB

- Use object like `(point-x #<POINT {10031070F3}>)`

- SPC mgg to edit defintion at point

- M-, to return to last point

- M-n, and M-p to jump thru notes of compile warnings

- ,h < who calls

- ,hH Hyperspec of function at point  

#### Slime Trace Dialog

- Trace function with C-c C-t, while on the function name.

- C-c, C-t to untrace.

- After execution, call Trace Dialog with C-c T.

- "Fetch next batch"

#### Redefining classes

- `(find-class 'point)`
- `Update-instance-for-redefined-class` to redefine a class dynamically

## Installation
 
