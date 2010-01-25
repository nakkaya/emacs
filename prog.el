;;
;; Programming Mode Custom Code
;;

(setq compilation-window-height 10)

(setq auto-mode-alist
      (append '(("\\.C$"       . c++-mode)
		("\\.cc$"      . c++-mode)
		("\\.c$"       . c-mode)
		("\\.markdown$"  . markdown-mode)
		("\\.h$"       . c++-mode)
		("\\.i$"       . c++-mode)
		("\\.ii$"      . c++-mode)
		("\\.m$"       . objc-mode)
		("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode)
		("\\.java$"    . java-mode)
		("\\.xml$"     . xml-mode)
		("\\.outline$" . outline-mode)
		("\\.sql$"     . c-mode)
		("\\.pde$"     . c++-mode)
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
(add-hook 'clojure-mode-hook 'lispy-parens)

(setq class-path (concat "-cp " 
			 "./extLibs/*:"
			 "../extLibs/*:"
			 "./classes/:"
			 "/Users/nakkaya/Projects/clodiuno/src/:"
			 "."))
(setq clojure-command (concat "/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home/bin/java -server -Dfile.encoding=UTF-8 "  class-path " clojure.lang.Repl"))

(setq lisp-programs 
      (list (list '"clojure" clojure-command)
	    (list '"sbcl" "/opt/local/bin/sbcl")))

(defun na-run-lisp (arg)
  (interactive "P")
  (if (null arg)
      (setq inferior-lisp-program (second (first lisp-programs)))
    (let (choice) 
      (setq choice (completing-read 
		    "Lisp: " 
		    (mapcar 'first lisp-programs)))
      (dolist (l lisp-programs)
	(if (string= (first l) choice)
	    (setq inferior-lisp-program (second l))))))
  (run-lisp inferior-lisp-program))

(defun na-load-buffer ()
  (interactive)
  (point-to-register 5)
  (mark-whole-buffer)
  (lisp-eval-region (point) (mark) nil)
  (jump-to-register 5))

;;sub process support for clojure
(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (define-key clojure-mode-map 
	       "\e\C-x" 'lisp-eval-defun)
	     (define-key clojure-mode-map 
	       "\C-x\C-e" 'lisp-eval-last-sexp)
	     (define-key clojure-mode-map 
	       "\C-c\C-e" 'lisp-eval-last-sexp)
	     (define-key clojure-mode-map 
	       "\C-c\C-r" 'lisp-eval-region)
	     (define-key clojure-mode-map 
	       "\C-c\C-l" 'na-load-buffer)
	     (define-key clojure-mode-map 
	       "\C-c\C-z" 'run-lisp)))

;;
;;java custom
;;
(add-hook 'java-mode-hook 'outline-minor-mode)

;;
;;git.el
;;
(require 'git)
(setq git-committer-name "Nurullah Akkaya")
(setq git-committer-email "nurullah@nakkaya.com")

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))
(setq exec-path (append exec-path '("/opt/local/bin")))


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

;;
;;run term with /bin/bash
(setq term-term-name "xterm-color")
(setq-default term-buffer-maximum-size 5000)

(defun na-linux-run-term ()
  "run bash"
  (interactive)
  (term "/bin/bash"))

(defun na-run-term-or-rename ()
  "create new shell or rename old"
  (interactive)  
  (if (not (eq (get-buffer "*terminal*")  nil ) )
      (progn
	( setq new-buffer-name (read-from-minibuffer "Name shell to: " ) )
	(set-buffer "*terminal*")
	( rename-buffer new-buffer-name )))
  
  (if (eq (get-buffer "*terminal*")  nil) 
      (progn
	(na-linux-run-term ))))

(defun na-switch-between-terminals () 
  "cycle multiple terminals"
  (interactive)
  (if (not (eq (or (get-buffer "*terminal*") 
		   (get-buffer "*inferior-lisp*")) nil))
      (progn     
	(setq found nil)
	(bury-buffer)
	(setq head (car (buffer-list)))      
	(while  (eq found nil)	
	  (set-buffer head)	
	  (if (or (eq major-mode 'term-mode)
		  (eq major-mode 'inferior-lisp-mode))
	      (setq found t)
	    (progn
	      (bury-buffer)
	      (setq head (car (buffer-list)))))))))

(defun na-run-git-switch ()
  "Switch to git buffer or run git-status"
  (interactive)  
  (if (not (eq (get-buffer "*git-status*") nil))
      (switch-to-buffer "*git-status*")
    (git-status (read-directory-name "Select Directory: "))))
