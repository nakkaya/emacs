;; -*- emacs-lisp -*-
;; .emacs
;; Nurullah Akkaya
;; nurullah@nakkaya.com
(setq load-path (cons "~/Projects/emacs/" load-path))
(setq load-path (cons "~/Projects/emacs/ext/" load-path))
(setq load-path (cons "~/Projects/emacs/ext/clojure-mode/" load-path))
(setq load-path (cons "~/Projects/emacs/int/" load-path))

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
(require 'diff-mode)
(load "nmap.el")
(load "dsniff.el")
(load "searchDown.el")

(load "skeletons.el")
(load "frame.el")
(load "jump.el")
(load "php-mode.el")
(load "clojure-mode.el")
(load "javadoc-help.el")
(load "prog.el")

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

;;
;; Mode Specific Code
;;
;;nmap mode configuration
(cond
 ((string= "linux" my-opsys)
  (setq nmap-nmap-program "/usr/bin/nmap")
  (setq nmap-ifconfig-program "/sbin/ifconfig")
  (setq nmap-network-interface "ath0")))

(cond
 ((string= "osx" my-opsys)
  (setq dsniff-os "osx")
  (setq dsniff-network-interface "en1")
  (setq dsniff-arpspoof-program "/opt/local/sbin/arpspoof")
  (setq dsniff-urlsnarf-program "/opt/local/sbin/urlsnarf")
  (setq dsniff-dsniff-program "/opt/local/sbin/dsniff")
  (setq dsniff-msgsnarf-program "/opt/local/sbin/msgsnarf")
  (setq dsniff-mailsnarf-program "/opt/local/sbin/mailsnarf")
  (setq dsniff-tcpdump-program "/opt/local/sbin/tcpdump")))

;;markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

;; Text
(delete-selection-mode)
(setq fill-column 80) ;;; Text lines limit to 80 chars
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;; ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Markup" (or (mode . org-mode)
			     (mode . html-mode)
			     (mode . markdown-mode)
			     (mode . xml-mode)
			     (name . "\\.xml$")
			     (mode . text-mode)))
	       ("Source" (or
			  (mode . java-mode)
			  (mode . clojure-mode)
			  (mode . php-mode)
			  (mode . ruby-mode)
			  (mode . shell-script-mode)
			  (mode . sh-mode)
			  (mode . c-mode)
			  (mode . lisp-mode)
			  (mode . cperl-mode)
			  (mode . asm-mode)
			  (mode . emacs-lisp-mode)
			  (mode . c++-mode)))

	       ("Terminal" (or (mode . term-mode)
			       (mode . inferior-lisp-mode)))
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
	       ("IRC" (or
			(mode . erc-mode)))
	       ("emacs" (or
			 (name . "^\\*info\\*$")
			 (name . "^\\*mpg123\\*$")
			 (name . "^\\.todo-do")
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Completions\\*$") 
			 (name . "^\\*Backtrace\\*$")
			 (name . "^TAGS$")
			 (name . "^\\*Help\\*$")
			 (name . "^\\*Shell Command Output\\*$")))))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-expert t)

;;
;; ERC
;;
(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
          '(("freenode.net" "#clojure") ))

;;; Finally, connect to the networks.
(defun na-erc ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.freenode.net" :port 6667
                :nick "hamza" :full-name "rgb"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;bindings of  keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [S-f1] 'na-run-term-or-rename)
(global-set-key [M-f1] 'run-lisp)
(global-set-key [f1] 'na-switch-between-terminals)
(global-set-key [M-f2] 'todo-show)
(global-set-key [f3] 'javadoc-lookup)
(global-set-key [f4] 'na-mpg123-run)
(global-set-key [f5] 'na-proxy-start)
(global-set-key [S-f5] 'na-proxy-stop)
(global-set-key [f6] 'nmap)
(global-set-key [S-f6] 'dsniff)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'toggle-truncate-lines)
(global-set-key [M-f12] 'na-switch-project)

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
(define-key global-map [(meta \[)] 'other-frame)
;;works for pc keyboard
(define-key global-map [C-M-kp-subtract] 'erase-buffer)
;;works for ibook keyboard
(define-key global-map [C-M-backspace] 'erase-buffer)

;;outline bindings
(global-set-key [M-up] 'hide-subtree)
(global-set-key [M-down] 'show-subtree)

;keybindindings for git
(global-set-key "\C-xgs" 'git-status)


(cond 
 ((string= "linux" my-opsys)
  (global-set-key [(control .)( control w )] 'na-resize-frame-bizdik))
 ((string= "osx" my-opsys)
  (global-set-key [(control .)( control w )] 'na-resize-frame-big-bf )
  (setq mac-option-modifier 'super )
  (setq mac-command-modifier 'meta )
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)))


(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))
 
(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

(add-hook 'clojure-mode-hook    'my-tab-fix)
(add-hook 'java-mode-hook    'my-tab-fix)
(add-hook 'c++-mode    'my-tab-fix)
(add-hook 'c-mode    'my-tab-fix)
(add-hook 'xml-mode    'my-tab-fix)


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

;;setup server calls
(add-hook 'server-visit-hook 'call-raise-frame)
(add-hook 'find-file-hook 'call-raise-frame)
(defun call-raise-frame ()
  (raise-frame))

(server-start)
