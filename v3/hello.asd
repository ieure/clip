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

(defsystem :hello/bin       ; The name HELLO/BIN indicates that this
                            ; is a secondary system of system HELLO.
  :depends-on (:hello)      ; This system needs the core HELLO system.
  :components ((:module :src
                :components ((:file "main"))))) ; ...and includes one
                                                ; additional file.
