(in-package :hello)

;; Unchanged from v1
(defun greet (whom)
  "Create a greeting message for WHOM."
  (format nil "Hello, ~A." whom))
