(defsystem :hello                       ; The system will be named
                                        ; HELLO, same as the project
  :serial t                             ; Load components in the same
                                        ; order they're defined.
  :components ((:file "packages")
               (:module "src" ; A module is a collection of pieces of
                              ; your program
                :components ((:file "greet"))))) ; Load the greet
                                                 ; function from
                                                 ; greet.lisp. The
                                                 ; file extension is
                                                 ; implied, and must
                                                 ; not appear here.

(defsystem :hello/bin
  :depends-on (:hello :unix-opts)       ; unix-opts dep added here
  :components ((:module :src
                :components ((:file "main")))))
