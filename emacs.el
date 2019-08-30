(setq package-list
      '(clojure-mode
	clojure-mode-extra-font-locking
	monroe
	python-mode
	matlab-mode
	yaml-mode
	org
	ob-ipython
	multi-term
	magit
	doom-themes))

;; list the repositories containing them
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;;;;Editor wide settings
(set-face-attribute 'default nil :height 120)

(delete-selection-mode)
(setq fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(blink-cursor-mode 1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1) 
(global-font-lock-mode t)
(setq make-backup-files nil)

(setq query-replace-highlight t)
(setq search-highlight t)

(global-auto-revert-mode 1)

(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;;;;Theme

(require 'doom-themes)
(load-theme 'doom-one t)

;;;;Org mode configuration

;; Enable Org mode
(require 'org)
(require 'ob-ipython)

(setq org-startup-with-inline-images t)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Run/highlight code using babel in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (ipython . t)
   ))

;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))

;;;;python
(require 'python)

(setq python-shell-interpreter "python3")

;;;;ibuffer

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Notes"
                (or (name . "^brain.org$")
                    (name . "^pass.gpg$")
                    (name . "^brain.org_archive$")
                    (name . "^bookmarks.org_archive$")))
               ("IRC" (mode . erc-mode))
               ("Notebooks" (or (mode . ein:notebooklist-mode)
                                (mode . ein:notebook-mode)
                                (mode . ein:notebook-multilang-mode)
                                (name . "^\\*ein:log-all\\*$")))
               ("Source" (or
                          (mode . java-mode)
                          (mode . clojure-mode)
                          (mode . org-mode)
                          (mode . bibtex-mode)
                          (mode . latex-mode)
                          (mode . xml-mode)
                          (mode . nxml-mode)
                          (mode . scheme-mode)
                          (mode . python-mode)
                          (mode . ruby-mode)
                          (mode . shell-script-mode)
                          (mode . sh-mode)
                          (mode . c-mode)
                          (mode . lisp-mode)
                          (mode . cperl-mode)
                          (mode . pixie-mode)
                          (mode . yaml-mode)
                          (mode . asm-mode)
                          (mode . emacs-lisp-mode)
                          (mode . c++-mode)
                          (mode . makefile-bsdmake-mode)
                          (mode . makefile-mode)
                          (mode . makefile-gmake-mode)
                          (mode . matlab-mode)
                          (mode . css-mode)))
               ("Terminal" (or (mode . term-mode)
                               (mode . inferior-lisp-mode)
                               (mode . inferior-python-mode)
                               (name . "^*MATLAB.*")
                               (name . "^*monroe.*")
                               (name . "^\\*offlineimap\\*$")))
               ("Dired" (or (mode . dired-mode) 
                            (mode . sr-mode)))
               ("Magit" (or (name . "^\\*magit.*\\*$")
                            (mode . magit-status-mode)
                            (mode . magit-diff-mode)
                            (mode . magit-process-mode)
                            (mode . magit-stash-mode)
                            (mode . magit-revision-mode)
                            (mode . magit-log-mode)))
               ("Emacs" (or
                         (name . "^\\*Process List\\*$")
                         (name . "^\\*Dired log\\*$")
                         (name . "^\\*info\\*$")
                         (name . "^\\*Man.*\\*$")
                         (name . "^\\*tramp.+\\*$")
                         (name . "^\\*trace.+SMTP.+\\*$")
                         (name . "^\\.todo-do")
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*git-status\\*$")
                         (name . "^\\*git-diff\\*$")
                         (name . "^\\*git-commit\\*$")
                         (name . "^\\*Git Command Output\\*$")
                         (name . "^\\*Org Export/Publishing Help\\*$")
                         (name . "^\\*Org-Babel Error Output\\*$")
                         (name . "^\\*Org PDF LaTeX Output\\*$")
                         (name . "^\\*Org Agenda\\*$")
                         (name . "^\\*Calendar\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Completions\\*$")
                         (name . "^\\*Warnings\\*$")
                         (name . "^\\*Org Agenda.*\\*$")
                         (name . "^\\*Org Help\\*$")
                         (name . "^\\*Backtrace\\*$")
                         (name . "^TAGS$")
                         (name . "^\\*Help\\*$")
                         (name . "^\\*Shell Command Output\\*$")
                         (name . "^\\*Calculator\\*$")
                         (name . "^\\*Calc Trail\\*$")
                         (name . "^\\*Compile-Log\\*$")))))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
       (lambda ()
         (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-expert t)

(setq ibuffer-formats '((mark modified read-only " "
                              (name 18 18 :left :elide)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))
;;;;Keybindings
(define-key global-map [(control \])] 'ibuffer)
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-map [(control \])] 'ibuffer)))

(global-set-key [f3] 'na-term-toggle-mode)

(global-set-key [(control \\)] 'other-window)
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-map [(control \\)] 'other-window)))

(global-set-key "\C-xgs" 'magit-status)
