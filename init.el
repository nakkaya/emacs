;; -*- emacs-lisp -*-
;; .emacs
;; Nurullah Akkaya
;; nurullah@nakkaya.com
(setq load-path (cons "~/.myEmacs/" load-path))
(setq load-path (cons "~/.myEmacs/ext/" load-path))
(setq load-path (cons "~/.myEmacs/ext/muse/lisp/" load-path))
(setq load-path (cons "~/.myEmacs/int/" load-path))


(cond
 ((string-match "GNU" (emacs-version))
  (cond 
   ((string-match "linux" system-configuration)
    (setq my-opsys "linux" ))
   ((string-match "nt" system-configuration)
    (setq my-opsys "windows"))
   ((string-match "apple" system-configuration)
    (setq my-opsys "osx" )))))

(require 'dired)
(load "nmap.el")
(load "dsniff.el")
(load "searchDown.el")

(load "initGnus.el")

(load "skeletons.el")
(load "linux.el")
(load "frame.el")
(load "jump.el")
(load "php-mode.el")
(load "javadoc-help.el")

;; ********************************************************
;; General customization
;; ********************************************************
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bookmark-default-file "~/.emacs.d/emacs.bmk")
 '(c++-default-style "stroustrup")
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(dired-dwim-target t)
 '(fill-column 72)
 '(inhibit-startup-screen t)
 '(next-line-add-newlines nil)
 '(query-replace-highlight t)
 '(require-final-newline t)
 '(scroll-step 1)
 '(search-highlight t)
 '(transient-mark-mode t))

(setq large-file-warning-threshold 256000000 )
;;debug
(setq debug-on-error t) 
;;enable blinking cursor
(blink-cursor-mode 1)
(put 'erase-buffer 'disabled nil)
;;disable scroll bar
(toggle-scroll-bar nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
;;do not continue line when split frame
(setq truncate-partial-width-windows nil)
;; turn on font-lock mode
(global-font-lock-mode t)
(when window-system
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions))

;;do not open a new frame for file
(cond 
 ((string= "osx" my-opsys)
  (setq ns-pop-up-frames nil)))

;;encoding system used
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;
;;Keyboard and Mouse custom
;;
;;mouse scrolling
;;map mouse wheels
(defun scroll-window-forward-line ()
  "Move window forward one line leaving cursor at relative position in window."
  (interactive)
  (scroll-up 1))

(defun scroll-window-backward-line ()
  "Move window backward one line leaving cursor at relative position in window."
  (interactive)
  (scroll-down 1)) 

(define-key global-map [wheel-up] 'scroll-window-backward-line)
(define-key global-map [wheel-down] 'scroll-window-forward-line)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;;
;;General Customization
;;

;;Recreate scratch if killed
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

;;
;;replace yes or no with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;;dired details mode
(require 'dired-details)
(dired-details-install)

(defun na-dired-up-directory-after-kill ()
  "Call 'dired-up-directory' after calling '(kill-buffer (current-buffer))'."
  (interactive)
  (let ((buf (current-buffer)))
    (dired-up-directory)
    (kill-buffer buf)))
(define-key dired-mode-map "\C-w" 'na-dired-up-directory-after-kill)

;;Colar on selection
(defface completion-setup-directory-face  '((t (:foreground "Blue")))
  "Face to use for directories."
  :group 'color-file-completion)

(defcustom color-file-completion-always t "If true, always turn on regexps in
completion buffers."
  :group 'color-file-completion
  :type 'boolean)

(defun completion-setup-directory-face()
  "When we are completing a filename, highlight directories."
  (interactive)
  ;;if this is completing a filename... highlight faces...
  (when (or color-file-completion-always
	    (eq minibuffer-completion-table 'read-file-name-internal))
    (let((font-lock-verbose nil))
      (font-lock-mode 1)
      (font-lock-add-keywords nil '(("[^ \n]+/" 0 'completion-setup-directory-face keep)))
      (font-lock-fontify-buffer))))

(add-hook 'completion-list-mode-hook 'completion-setup-directory-face)

(defun na-reopen-file ()
  "Reopen file in buffer."
  (interactive)
  (let ((p (point)))
    (progn
      (find-alternate-file buffer-file-name)
      (goto-char p) ) ) )

(setq auto-save-list-file-prefix "~/.saves/auto-save-list/" )
;;backup custom..
(setq
 backup-by-copying t			; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))			; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)			; use versioned backups
(setq tramp-auto-save-directory "~/.saves/tramp-autosave")

;;do not save tramp files
(defun tv-list-tramp-buffer-file-name ()
  (let* ((desktop-info-list (mapcar #'desktop-buffer-info (buffer-list)))
	 (tramp-buf-list (loop for i in desktop-info-list
			       if (and (listp i)
				       (stringp (car (nth 8 i)))
				       (string-match "^/su:.*\\|^/sudo:.*\\|^/scp:.*" (car (nth 8 i))))
			       collect (nth 2 i))))
    tramp-buf-list))

(add-hook 'desktop-save-hook #'(lambda ()
				 (let ((del-buf-list
					(tv-list-tramp-buffer-file-name)))
				   (dolist (i del-buf-list)
				     (kill-buffer i)))))

;; ********************************************************
;; Programming Customization
;; ********************************************************
(setq auto-mode-alist
      (append '(("\\.C$"       . c++-mode)
		("\\.cc$"      . c++-mode)
		("\\.c$"       . c-mode)
		("\\.h$"       . c++-mode)
		("\\.i$"       . c++-mode)
		("\\.ii$"      . c++-mode)
		("\\.m$"       . objc-mode)
		("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode)
		("\\.java$"    . java-mode)
		("\\.outline$" . outline-mode)
		("\\.sql$"     . c-mode)
		("\\.sh$"      . shell-script-mode)
		("\\.command$"      . shell-script-mode)
		("\\.mak$"     . makefile-mode)
		("\\.rb$"     . ruby-mode)
		("\\.php$"     . php-mode)
		("\\.GNU$"     . makefile-mode)
		("makefile$"   . makefile-mode)
		("Imakefile$"  . makefile-mode)
		("\\.Xdefaults$"    . xrdb-mode)
		("\\.Xenvironment$" . xrdb-mode)
		("\\.Xresources$"   . xrdb-mode)
		("*.\\.ad$"         . xrdb-mode)
		("\\.[eE]?[pP][sS]$" . ps-mode)
		("\\.zip$"     . archive-mode)
		("\\.tar$"     . tar-mode)
		("\\.tar.gz$"     . tar-mode)
		) auto-mode-alist))

(setq compilation-window-height 10)
(defun na-bounce-sexp ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
	  ((string-match "[\]})>]" prev-char) (backward-sexp 1))
	  (t (error "%s" "Not on a paren, brace, or bracket")))))


(defun na-uncomment-region (beg end &optional arg)
  (interactive "*r\np")
  (comment-region beg end (- arg)))
;;
;;c++ custom
;;
;;compile command
(require 'compile)
(add-hook 'c++-mode-hook
	  (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
		   (let ((file (file-name-nondirectory buffer-file-name)))
		     (concat "g++ -g -O2 -Wall -o " 
			     (file-name-sans-extension file)
			     " " file))))))

;;
;;java custom
;;
;; (defun kgold-setup-java ()
;;   (make-local-variable 'compile-command)
;;   (setq compile-command (concat "javac " (file-name-nondirectory 
;; 					  buffer-file-name))))
(defun kgold-setup-java ()
  (make-local-variable 'compile-command)
  (setq compile-command (concat "ant run -find")))
(add-hook 'java-mode-hook 'kgold-setup-java)

(defun na-java-lookup( query )
(interactive "sClass:")
(setq qList (split-string query ))
(setq url "http://www.google.com/search?q=")
(dolist (term qList )
  (setq url (concat url term "+")))
(setq url (substring url 0 (- (string-width url) 1 ) ))
(setq url (concat url "&sitesearch=java.sun.com/j2se/1.5.0/docs/api/"))
(browse-url url))

;;
;;Perl custom
;;
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
;;
;;Ruby Custom
;;
;;
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(require 'inf-ruby)
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)))

;;
;;lisp customization
;;
(defun lispy-parens ()
  "Setup parens display for lisp modes"
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1)
  (set-face-background 'show-paren-match-face (face-background 'default))
  (if (boundp 'font-lock-comment-face)
      (set-face-foreground 'show-paren-match-face 
			   (face-foreground 'font-lock-comment-face))
    (set-face-foreground 'show-paren-match-face 
			 (face-foreground 'default)))
  (set-face-foreground 'show-paren-match-face "red")
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold))
(add-hook 'lisp-mode-hook 'lispy-parens)
(add-hook 'emacs-lisp-mode-hook 'lispy-parens)
(add-hook 'lisp-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)

;;
;;Php
;;
;disable warnings
(add-hook 'php-mode-hook (lambda () (setq php-warned-bad-indent t)))

;;
;; Text
;;
(delete-selection-mode)
(setq fill-column 80) ;;; Text lines limit to 80 chars
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;
;;Mode Customization
;;

;;outline mode
(add-hook 'java-mode-hook 'outline-minor-mode)


;;Flyspell
(setq ispell-program-name "/opt/local/bin/ispell")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'java-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'lisp-mode-hook 'flyspell-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-mode)
(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

;;EasyPG
(cond 
 ((not(string= "windows" my-opsys))
  (require 'epa)
  (epa-file-enable)))

(cond 
 ((string= "osx" my-opsys)
  (setq epg-gpg-program "/opt/local/bin/gpg")))

;;doc-view

(cond 
 ((not(string= "windows" my-opsys))
  (require 'doc-view)))

(cond
 ((string= "osx" my-opsys)
  (setq doc-view-ghostscript-program "/opt/local/bin/gs")
  (setq doc-view-cache-directory "~/.doc-view/" ))
 ((string= "linux" my-opsys)
  (setq doc-view-ghostscript-program "/usr/bin/gs")
  (setq doc-view-cache-directory "~/.doc-view/" )))

(cond 
 ((not(string= "windows" my-opsys))
  (setq doc-view-ghostscript-options '("-dSAFER" "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4" "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET" ) )))

;;muse mode
(require 'muse-mode)
(require 'muse-publish)
(require 'muse-html)  ;; and so on
(add-hook 'muse-mode-hook 'abbrev-mode)

(setq muse-project-alist
      '(("nakkaya.com" ("~/Projects/nakkaya.com/muse/" :default "index")
         (:base "my-page-html" :path "~/Projects/nakkaya.com/html/"))
	("wiki" ("~/Projects/wiki/" :default "index"))))

(muse-derive-style "my-page-html" "html"
                   :header "~/Projects/nakkaya.com/muse/header.tmpl"
                   :footer "~/Projects/nakkaya.com/muse/footer.tmpl")

;; ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Muse" (mode . muse-mode))
	       ("Source" (or
			  (mode . java-mode)
			  (mode . php-mode)
			  (mode . ruby-mode)
			  (mode . shell-script-mode)
			  (mode . sh-mode)
			  (mode . c-mode)
			  (mode . lisp-mode)
			  (mode . cperl-mode)
			  (mode . asm-mode)
			  (mode . emacs-lisp-mode)
			  (mode . muse-mode)
			  (mode . c++-mode)))

	       ("Terminal" (mode . term-mode))
	       ("Network" (or 
			   (name . "^ssh.*$")
			   (name . "^\\*nmap\\*$")
			   (name . "^\\*dsniff\\*$")
			   (name . "^\\*ftp.+\\*$")
			   (name . "^\\*nmap.+\\*$")
			   (name . "^\\*arpspoof.+\\*$")
			   (name . "^\\*tramp.+\\*$")
			   (name . "^\\*trace.+SMTP.+\\*$")
			   (mode . dsniff-mode)
			   (mode . nmap-mode)))
	       ("dired" (mode . dired-mode))
	       ("gnus" (or
			(mode . message-mode)
			(mode . mail-mode)
			(mode . gnus-group-mode)
			(mode . gnus-summary-mode)
			(mode . gnus-article-mode)
			(name . "^\\.newsrc-dribble")))
	       ("emacs" (or
			 (name . "^\\*info\\*$")
			 (name . "^\\*mpg123\\*$")
			 (name . "^\\.todo-do")
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Completions\\*$") 
			 (name . "^\\*Backtrace\\*$")
			 (name . "^\\*Help\\*$")
			 (name . "^\\*Shell Command Output\\*$")))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-expert t)
;
;speedbar
;
(require 'speedbar)
(setq speedbar-frame-parameters '((minibuffer)
 (width . 40)
 (height . 20)
 (border-width . 0)
 (menu-bar-lines . 0)
 (tool-bar-lines . 0)
 (unsplittable . t)
 (left-fringe . 0)
 (top . 0)
 (left . 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;bindings of  keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [S-f1] 'na-run-term-or-rename)
(global-set-key [f1] 'na-switch-between-terminals)
(global-set-key [S-f2] 'gnus)
(global-set-key [f2] 'switch-to-gnus)
(global-set-key [f3] 'javadoc-lookup)
(global-set-key [S-f3] 'na-java-lookup)
(global-set-key [f4] 'na-mpg123-run)
(global-set-key [f5] 'na-proxy-start)
(global-set-key [S-f5] 'na-proxy-stop)
(global-set-key [f6] 'nmap)
(global-set-key [S-f6] 'dsniff)
(global-set-key [f7] 'outline-minor-mode)
(global-set-key [f8] 'toggle-truncate-lines)
(global-set-key [M-f12] 'na-switch-project)
(global-set-key [f12] 'compile)

(define-key global-map [(control meta .)] 'find-tag-other-window)
(global-set-key [(meta g)] 'goto-line)
(define-key global-map [(meta \])] 'ibuffer)
(add-hook 'term-mode-hook
	  '(lambda ()
	     (define-key term-raw-map [(meta \])] 'ibuffer)))

(add-hook 'term-mode-hook
	  '(lambda ()
	     (define-key term-raw-map [(meta .)] 'find-tag)))

(define-key global-map [(meta p)] 'na-cm-rotate)
(define-key global-map [(meta control  p)] 'na-cm-save-point)
(global-set-key [(control =)] 'na-bounce-sexp)
(global-set-key [M-right] 'other-window)
;;works for pc keyboard
(define-key global-map [C-M-kp-subtract] 'erase-buffer)
;;works for ibook keyboard
(define-key global-map [C-M-backspace] 'erase-buffer)

;;outline bindings
(global-set-key [M-up] 'hide-subtree)
(global-set-key [M-down] 'show-subtree)


(cond 
 ((string= "linux" my-opsys)
  (global-set-key [(control .)( control w )] 'na-resize-frame-bizdik))
 ((string= "osx" my-opsys)
  (global-set-key [(control .)( control w )] 'na-resize-frame-big-bf )
  (setq mac-option-modifier 'super )
  (setq mac-command-modifier 'meta )
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)))


;;
;; Session Customization
;;
;;destop mode
(load "desktop")
(desktop-load-default)
;; automatically save the desktop on exit.
(setq desktop-enable t)
;;save where we left cursor in file on kill
(require 'saveplace)
(setq-default save-place t)
;;save recent opened files
;(require 'recentf)
;(recentf-mode 1)
(setq bookmark-save-flag 1 )

(load "projects.el")

(cond
 ((string= "osx" my-opsys)
  (na-resize-frame-big)
  (split-window-horizontally))
((string= "linux" my-opsys)
  (na-resize-frame-bizdik))
((string= "windows" my-opsys)
  (na-frame-windows)))

(server-start)
