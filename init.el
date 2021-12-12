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
	cider
	python-mode
	python-cell
	matlab-mode
	yaml-mode
	markdown-mode
        terraform-mode
	flycheck
        lsp-mode
	lsp-ui
        lsp-java
	org
        poly-org
        org-superstar
	pdf-tools
        saveplace-pdf-view
        org-noter
        docker
        dockerfile-mode
        docker-compose-mode
	multi-term
	magit
	magit-todos
	popper
        doom-themes
        doom-modeline
	company
        projectile
        helm
        quelpa))

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
