(setq package-list
      '(use-package
         jupyter))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'jupyter)
(require 'ob-jupyter)

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((jupyter . t))))
