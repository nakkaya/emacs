(custom-set-variables
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))

(setq dotfiles-dir
      (file-name-directory
       (or load-file-name (buffer-file-name))))

(setq package-list
      '(use-package
        undo-tree
        hl-todo
        clojure-mode
	clojure-mode-extra-font-locking
	html-to-hiccup
	cider
	python-mode
	python-cell
	matlab-mode
	yaml-mode
	markdown-mode
        terraform-mode
	pine-script-mode
	flycheck
        lsp-mode
	lsp-ui
        lsp-java
	org
        org-superstar
	ob-async
        docker
        dockerfile-mode
        docker-compose-mode
	multi-term
	magit
	popper
        doom-themes
        doom-modeline
	all-the-icons
	company
        projectile
        helm
        quelpa
	elfeed
	elfeed-protocol
	transmission))

(when (or (eq system-type 'gnu/linux)
	  (eq system-type 'darwin))

  (push 'magit-todos package-list)
  (push 'pdf-tools package-list)
  (push 'saveplace-pdf-view package-list)
  (push 'org-noter package-list))

(when module-file-suffix
  (push 'jupyter package-list))

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

;; Load Private Config If Present
(load "~/.netrc.el" t)
