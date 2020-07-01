(defsystem :hello
  :components ((:file "packages")
               (:module "src"
                        :serial t
                        :components ((:file "greet")))))


(defsystem :hello/bin
  :depends-on (:hello)      ; This system needs the core HELLO system…
  :components ((:module :src
                :components ((:file "main"))))) ; …and includes one
                                                ; additional file.
