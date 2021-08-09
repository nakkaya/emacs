(setq package-list
      '(use-package
           elfeed
           jupyter
           pdf-tools
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

(use-package org-noter
  :after (:any org pdf-tools)
  :config
  (setq
   org-noter-always-create-frame nil
   org-noter-hide-other t
   org-noter-notes-search-path na-agenda-folder
   org-noter-auto-save-last-location t)
  :ensure t)

;; elfeed

(setq elfeed-db-directory "/storage/.elfeed")

(setq elfeed-feeds
      '("https://news.ycombinator.com/rss"
        "https://www.reddit.com/r/lisp/.rss"))

;; Init
;;

(setq server-socket-dir "/opt/emacsd/server")
(setq server-name "emacsd")
(defun server-ensure-safe-dir (dir) "Noop" t)
(server-start)
(set-face-attribute 'default nil :height 125)
(blink-cursor-mode)
