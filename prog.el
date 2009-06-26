;;
;; Programming Mode Custom Code
;;

(setq compilation-window-height 10)

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
(add-hook 'java-mode-hook 'outline-minor-mode)
;; (defun na-setup-java ()
;;   (make-local-variable 'compile-command)
;;   (setq compile-command (concat "javac " (file-name-nondirectory 
;; 					  buffer-file-name))))
(defun na-setup-java ()
  (make-local-variable 'compile-command)
  (setq compile-command (concat "ant run -find")))
(add-hook 'java-mode-hook 'na-setup-java)

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
;;git.el
;;
(require 'git)
(setq git-committer-name "Nurullah Akkaya")
(setq git-committer-email "nurullah@nakkaya.com")

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

(setq exec-path (append exec-path '("/opt/local/bin")) )


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
