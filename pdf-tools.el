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
  (pdf-tools-install :no-query)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package org-noter
  :after (:any org pdf-tools)
  :config
  (setq
   org-noter-always-create-frame nil
   org-noter-hide-other t
   org-noter-notes-search-path na-agenda-folder
   org-noter-auto-save-last-location t)
  :ensure t)
