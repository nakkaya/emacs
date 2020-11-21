(setq dotfiles-dir
      (file-name-directory
       (or load-file-name (buffer-file-name))))

(setq package-list
      '(use-package
        clojure-mode
	clojure-mode-extra-font-locking
	monroe
	python-mode
	matlab-mode
	yaml-mode
	markdown-mode
	org
        docker
        dockerfile-mode
        docker-compose-mode
	multi-term
	magit
        transmission
	doom-themes
	smart-tab
        helm
        evil
        quelpa
        ein))

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'org)
(require 'ob-tangle)

(org-babel-load-file
 (expand-file-name "emacs.org" dotfiles-dir))
