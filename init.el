(custom-set-variables
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))

(setq dotfiles-dir
      (file-name-directory
       (or load-file-name (buffer-file-name))))

(setq package-list
      '(use-package
        quelpa
        quelpa-use-package
        clipetty
        undo-tree
        persp-mode
        hl-todo
        outline-indent
        clojure-mode
        clojure-mode-extra-font-locking
        cider
        python-mode
        conda
        yaml-mode
        markdown-mode
        jsonnet-mode
        terraform-mode
        flycheck
        yasnippet
        lsp-mode
        lsp-ui
        org
        ein
        dockerfile-mode
        docker-compose-mode
        multi-term
        eat
        magit
        kubernetes
        forge
        popper
        doom-themes
        doom-modeline
        all-the-icons
        company
        projectile
        helm
        helm-org-rifle
        gptel))

(when (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))

  (dolist (pkg '(magit-todos
		 pdf-tools
		 saveplace-pdf-view
                 org-noter
		 org-pdftools
		 org-roam))
    (push pkg package-list)))

(when module-file-suffix
  (dolist (pkg '(jupyter vterm))
    (push pkg package-list)))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless (cl-every 'package-installed-p package-list)
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(when (eq system-type 'darwin)
  (setq-default quelpa-build-tar-executable "/opt/homebrew/opt/gnu-tar/bin/gtar"))

(require 'quelpa-use-package)
(require 'org)
(require 'ob-tangle)

(org-babel-load-file
 (expand-file-name "emacs.org" dotfiles-dir))

;; Load Private Config If Present
(load "~/.netrc.el" t)
