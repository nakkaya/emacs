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
		("\\.xml$"     . xml-mode)
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
(add-hook 'clojure-mode-hook 'abbrev-mode)

;;
;; Clojure
;;"-Djava.awt.headless=true "
(setq class-path (concat "-cp " 
			 "/java/clojure/clojure.jar:"
			 "/java/clojure/clojure-contrib.jar:"
					;compojure related
			 "./extLibs/compojure.jar:"
			 "./extLibs/commons-codec-1.3.jar:"
			 "./extLibs/commons-fileupload-1.2.1.jar:"
			 "./extLibs/commons-io-1.4.jar:"
			 "./extLibs/grizzly-http-servlet-1.9.10.jar:"
			 "./extLibs/grizzly-http-webserver-1.9.10.jar:"
			 "./extLibs/jetty-6.1.15.jar:"
			 "./extLibs/jetty-util-6.1.15.jar:"
			 "./extLibs/servlet-api-2.5-20081211.jar:"
			 "./extLibs/dsn.jar:"
			 "./extLibs/activation.jar:"
			 "./extLibs/imap.jar:"
			 "./extLibs/mail.jar:"
			 "./extLibs/mailapi.jar:"
			 "./extLibs/pop3.jar:"
			 "./extLibs/smtp.jar:"		
					; layout
			 "/java/form/jfd-loader.jar:"
			 "/java/form/swing-layout.jar:"
			 "../extLibs/miglayout-3.7-swing.jar:"
			 "./extLibs/miglayout-3.7-swing.jar:"
			 "./extLibs/bcprov-jdk15-143.jar:"
			 "../extLibs/mac-widgets.jar:"
			 "./extLibs/mac-widgets.jar:"
			 "./extLibs/forms.jar:"
			 "../extLibs/forms.jar:"
			 "."))
(setq clojure-command (concat "/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home/bin/java -server " class-path " clojure.lang.Repl" ))
(setq inferior-lisp-program   clojure-command)
(add-hook 'clojure-mode-hook 'lispy-parens)

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

;;
;;print ascii table
;;
(defun ascii-table ()
    "Display basic ASCII table (0 thru 128)."
    (interactive)
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (save-excursion (let ((i -1))
      (insert "ASCII characters 0 thru 127.\n\n")
      (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
      (while (< i 31)
        (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                        (setq i (+ 1  i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)
                        (setq i (+ 32 i)) i (single-key-description i)))
        (setq i (- i 96))))))
