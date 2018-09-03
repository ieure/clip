(defpackage :hello                      ; Define a package and name it HELLO
  (:use :common-lisp)                   ; The package needs Common Lisp
  (:export :greet :main))               ; This package has two public
                                        ; symbols, GREET and MAIN.
