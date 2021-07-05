(setq package-list
      '(use-package
         pdf-tools
         org-noter))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

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
