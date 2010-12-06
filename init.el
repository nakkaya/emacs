;; init.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org-mode" (expand-file-name
                                        "ext" dotfiles-dir))))
;; Load up Org Mode and Babel
(require 'org-install)
(require 'ob-tangle)

;; load up the main file
(org-babel-load-file (expand-file-name "babel.org" dotfiles-dir))

;;; init.el ends here
