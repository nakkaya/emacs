(setq package-list
      '(use-package
         pdf-tools
         org-noter))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(use-package pdf-tools
 :config
 ;; initialise
 (pdf-tools-install)
 ;; open pdfs scaled to fit page
 (setq-default pdf-view-display-size 'fit-page)
 ;; automatically annotate highlights
 (setq pdf-annot-activate-created-annotations t)
 ;; use normal isearch
 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
