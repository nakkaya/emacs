(setq package-list
      '(use-package
         elfeed))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

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
