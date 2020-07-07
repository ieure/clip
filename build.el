(require 'org)
(require 'ob-shell)
(require 'ob-lisp)
(require 'ox-org)

(add-to-list 'org-babel-load-languages '(shell . t))
(add-to-list 'org-babel-load-languages '(lisp . t))
(setq inferior-lisp-program "sbcl"
      org-confirm-babel-evaluate nil)

(find-file "readme.org")
(with-current-buffer (org-org-export-as-org nil nil t nil)
  (write-file "README.org"))
