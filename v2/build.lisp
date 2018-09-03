(load "packages.lisp")                  ; Load package definition
(load "src/greet.lisp")                 ; Load the core
(load "src/main.lisp")                  ; Load the toplevel

;; Unchanged from v1
(sb-ext:save-lisp-and-die "hello"
 :toplevel 'hello:main
 :executable t)
