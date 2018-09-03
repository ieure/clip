(in-package :hello)

(unix-opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help"))

(defun main ()
  "Greet someone, or something."
  (multiple-value-bind (options free-args)
      (unix-opts:get-opts)
    (if (or (getf options :help) (/= (length free-args) 1))
        (unix-opts:describe
         :prefix "A Hello World program."
         :args "WHOM")
        (write-line (greet (car free-args)))))

  (uiop:quit))
