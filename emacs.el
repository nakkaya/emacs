(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq fill-column 80)
(delete-selection-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-font-lock-mode t)
(setq make-backup-files nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(global-auto-revert-mode 1)

(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

(setq frame-title-format (list "GNU Emacs " emacs-version))
(set-frame-font "DejaVu Sans Mono 11" nil t)

(blink-cursor-mode 1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq-default mode-line-format
	      '(""
		mode-line-modified
		(-3 . "%p") ;; position
		" - %b - "
		mode-name
		mode-line-process
		minor-mode-alist
		"%n" " - "
		(line-number-mode "L%l ")
		(column-number-mode "C%c ")))

(require 'doom-themes)
(load-theme 'doom-one t)

(require 'ob-ipython)

(setq org-hide-leading-stars t)
(setq org-return-follows-link t)
(setq org-startup-with-inline-images t)
(setq org-src-window-setup 'current-window)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-babel-python-command "python3")

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

(require 'python)
(setq python-shell-interpreter "python3")

(require 'dired)

(setq dired-recursive-deletes 'always)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode)))

(defun na-dired-up-directory-after-kill ()
  "Call 'dired-up-directory' after calling '(kill-buffer (current-buffer))'."
  (interactive)
  (let* ((buf (current-buffer))
	 (kill-curr (if (= (length (get-buffer-window-list buf)) 
			   1)
			t nil)))
    (dired-up-directory)
    (when kill-curr
      (kill-buffer buf))))

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

(define-key dired-mode-map "\C-w" 'na-dired-up-directory-after-kill)
