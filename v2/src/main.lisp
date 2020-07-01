(in-package :hello)

;; Unchanged from v1
(defun main ()
  "Greet someone, or something."
  (write-line (greet (first (uiop:command-line-arguments))))

  (uiop:quit))
