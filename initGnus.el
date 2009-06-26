(require 'gnus)
(require 'nnrss)
(require 'browse-url)
;;
;;Gnus
;;
(setq gnus-novice-user nil)
(setq user-full-name "Nurullah Akkaya")
(setq user-mail-address "nurullah@nakkaya.com")
(setq mail-user-agent 'gnus-user-agent)

;;storage
(setq gnus-directory "~/.gnus.d")
(setq message-directory "~/.gnus.d/mail")
;; (setq nnml-directory "~/.gnus.d/nnml-mail")
(setq gnus-article-save-directory "~/.gnus.d/saved")
(setq gnus-kill-files-directory "~/.gnus.d/scores")
(setq gnus-cache-directory "~/.gnus.d/cache")
(setq message-auto-save-directory "~/.gnus.d")

;; General speedups.
(setq gnus-check-new-newsgroups nil) 
(setq gnus-nov-is-evil nil) 
(setq gnus-save-newsrc-file t)
(setq gnus-interactive-exit nil)
(setq gnus-activate-level 1)
(setq gnus-use-cache t)
(setq message-from-style 'angles) 
(setq gnus-summary-line-format "%U%R%z%d %I%(%[%3L: %-10,10n%]%) %s\n")
(setq gnus-agent nil)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; Inline images?
(setq mm-attachment-override-types '("image/.*"))
;; No HTML mail
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))


;; Never show vcard stuff, I never need it anyway
(setq gnus-ignored-mime-types
      '("text/x-vcard"))

(setq gnus-posting-styles
      '((".*"
	 (signature "Nurullah Akkaya\nhttp://nakkaya.com"))))

(defun add-mail-headers ()
  "Add Reply-To, X-OS, and X-Uptime mail headers."
  (message-add-header
   (concat "X-Homepage: http://nakkaya.com")
;;    (concat "X-OS: "
;;            (substring (shell-command-to-string "uname -a") 0 -1))
;;    (concat "X-Uptime: "
;;            (substring (shell-command-to-string "uptime") 0 -1))
))
(add-hook 'message-send-hook 'add-mail-headers)

(setq gnus-visible-headers 
      (mapconcat 'regexp-quote
                 '("From:" "Newsgroups:" "Subject:" "Date:" 
                   "Organization:" "To:" "Cc:" "Followup-To" 
		   "Gnus-Warnings:"
                   "X-Sent:" "X-URL:" "User-Agent:" "X-Newsreader:"
                   "X-Mailer:" "Reply-To:"
                   "X-Attachments" "X-Diagnostic")
                 "\\|"))

;;* Higher Scoring of followups to myself
;;*================================
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
;; Use a second connection to grab the next article when I read one, so
;; I don't have to wait for it be downloaded.  
;(setq gnus-asynchronous t)
;; Configure incoming mail (IMAP)
(load "tls")
(setq gnus-select-method '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-server-port 993)
		      (nnimap-authinfo-file "~/.authinfo")
		      (nnimap-stream ssl)))

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      starttls-gnutls-program "/usr/bin/gnutls-cli"
      starttls-extra-arguments nil      
      smtpmail-gnutls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      ;; smtpmail-auth-credentials
      ;;'(("smtp.gmail.com" 587 "nurullah@nakkaya.com" "pass" ))
      smtpmail-starttls-credentials 
      '(("smtp.gmail.com" 587 "nurullah@nakkaya.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      smtpmail-local-domain "nakkaya.com")

(require 'smtpmail)
(add-hook 'mail-mode-hook 'mail-abbrevs-setup)
(setq message-kill-buffer-on-exit t)

(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)


(defun na-gmail-move-spam ()
(interactive)
(gnus-summary-move-article "[Gmail]/Spam"))

(defun na-gmail-move-trash ()
(interactive)
(gnus-summary-move-article nil "[Gmail]/Trash"))

(define-key gnus-summary-mode-map [(v)] 'na-gmail-move-trash)

;;
;;Exit gnus on emacs exit
;;
;; (require 'advice)
;; ; define a wrapper around the save-buffers-kill-emacs
;; ; to run the new hook before:
;; (defadvice save-buffers-kill-emacs
;;   (before my-save-buffers-kill-emacs activate)
;;   "Run before-kill-emacs-hook before asking to save this and that."
;;   (run-hooks 'before-kill-emacs-hook))
;; ; use that hook for its purpose
;; (add-hook 'before-kill-emacs-hook 'gnus-group-exit)

;; Remove HTML tags from a buffer
(defun na-wash-ugly-html ()
  "Remove ugly HTML tags"
  (interactive)
  (toggle-read-only -1)
  (save-excursion    
    (beginning-of-buffer)
    (na-wash-remove-style)
    (beginning-of-buffer)
    (while (re-search-forward "<[^<@>]*>" nil t)
      (replace-match "" nil nil))
;;     (beginning-of-buffer)
;;     (while (re-search-forward "\n+" nil t)
;;       (replace-match "\n" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "&gt;" nil t)
      (replace-match ">" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "&lt;" nil t)
      (replace-match "<" nil nil))
    (beginning-of-buffer)
    (while (re-search-forward "&.*;" nil t)
      (replace-match "" nil nil))))

(defun na-wash-remove-style()
(interactive)
(setq code-begin (re-search-forward "<!--" nil t))
(setq code-end (re-search-forward "-->" nil t))
(if (not (eq code-begin nil ) )
(delete-region code-begin code-end )))
;;remove html from gnus article buffer
(defun na-gnus-article-clean ()
  "Remove ugly HTML tags"
  (interactive)
  (set-buffer gnus-article-buffer)
  (na-wash-ugly-html))

(defun switch-to-gnus (&optional arg)
  "Switch to a Gnus related buffer.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*

    Use a prefix argument to start Gnus if no candidate exists."
  (interactive "P")
  (let (candidate
	(alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
		 ("^\\*Group")
		 ("^\\*Summary")
		 ("^\\*Article" nil 
		  (lambda ()				      
		    (buffer-live-p 
		     gnus-article-current-summary))))))
    (catch 'none-found
      (dolist (item alist)
	(let (last
	      (regexp (nth 0 item))
	      (optional (nth 1 item))
	      (test (nth 2 item)))
	  (dolist (buf (buffer-list))
	    (when (and (string-match regexp (buffer-name buf))
		       (> (buffer-size buf) 0))
	      (setq last buf)))
	  (cond ((and last (or (not test) (funcall test)))
		 (setq candidate last))
		(optional
		 nil)
		(t
		 (throw 'none-found t))))))
    (cond (candidate
	   (switch-to-buffer candidate))
	  (arg
	   (gnus))
	  (t
	   (error "No candidate found")))))
