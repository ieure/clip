(load "hello.lisp")                     ; Load the code into the Lisp
                                        ; environment

(sb-ext:save-lisp-and-die "hello"       ; Save a Lisp image
 :toplevel 'hello:main                  ; The toplevel function is
                                        ; MAIN, inside the HELLO
                                        ; package.
 :executable t)                         ; Make an executable.
