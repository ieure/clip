(defpackage :hello
  (:use :common-lisp)
  (:export :greet :main))

(in-package :hello)

(defun greet (whom)
  "Create a greeting message for WHOM."
  (format nil "Hello, ~A." whom))

(defun main ()
  "Greet someone, or something."
  (write-line (greet (first (uiop:command-line-arguments))))

  (uiop:quit))
