(setq package-list
      '(use-package
           elfeed
           jupyter
           pdf-tools
           saveplace-pdf-view
           org-noter))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Emacs Jupyter
;;

(require 'jupyter)
(require 'ob-jupyter)

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((jupyter . t))))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:session . "py")
                                                     (:results . "raw drawer")))

;; PDF Tools
;;

(use-package pdf-tools
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(require 'saveplace-pdf-view)

(use-package org-noter
  :after (:any org pdf-tools)
  :config
  (setq
   org-noter-always-create-frame nil
   org-noter-hide-other t
   org-noter-notes-search-path (list na-agenda-folder)
   org-noter-auto-save-last-location t)
  :ensure t)

;; elfeed

(require 'elfeed)

(setq elfeed-db-directory "/storage/.elfeed"
      elfeed-sort-order   'ascending)

(run-with-idle-timer (* 2 60 60) t #'elfeed-update)

(define-key elfeed-show-mode-map (kbd "j") 'elfeed-show-next)
(define-key elfeed-show-mode-map (kbd "k") 'elfeed-show-prev)

;; Load Private Config
;;

(load "~/.netrc.el" t)

;; Init
;;

(setenv "EDITOR" "edit")
(setq frame-title-format (list "emacsd"))
(set-face-attribute 'default nil :height 125)
(blink-cursor-mode)
