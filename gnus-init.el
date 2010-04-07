(require 'gnus)
(require 'browse-url)
;;
;;Gnus
;;
(setq gnus-novice-user nil)
(setq user-full-name "Nurullah Akkaya")
(setq user-mail-address "nurullah@nakkaya.com")
(setq mail-user-agent 'gnus-user-agent)
;;storage
(setq gnus-directory "~/Documents/gnus")
(setq message-directory "~/Documents/gnus/mail")
(setq gnus-article-save-directory "~/Documents/gnus/saved")
(setq gnus-kill-files-directory "~/Documents/gnus/scores")
(setq gnus-cache-directory "~/Documents/gnus/cache")
(setq message-auto-save-directory "~/Documents/gnus")

;; General speedups.
(setq gnus-save-newsrc-file t)
(setq gnus-interactive-exit nil)
(setq message-from-style 'angles) 
(setq gnus-summary-line-format "%U%R%z%d %I%(%[%3L: %-10,10n%]%) %s\n")
(setq gnus-agent nil)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; Inline images?
(setq mm-attachment-override-types '("image/.*"))
;; No HTML mail
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;;threading
(setq gnus-show-threads t
      gnus-thread-hide-subtree t	;all threads will be hidden
      gnus-thread-hide-killed t
      ;; if t, the changed subject in the  middle of a thread is ignored.
      ;; default nil and the change accepted.
      gnus-thread-ignore-subject t
      ;;default 4
      gnus-thread-indent-level 2)

(define-key gnus-summary-mode-map [(right)] 'gnus-summary-show-thread)
(define-key gnus-summary-mode-map [(left)]  'gnus-summary-hide-thread)

;; Never show vcard stuff, I never need it anyway
(setq gnus-ignored-mime-types '("text/x-vcard"))

(setq gnus-posting-styles
      '((".*" (signature "Nurullah Akkaya\nhttp://nakkaya.com"))))

(defun add-mail-headers ()
  (message-add-header
   (concat "X-Homepage: http://nakkaya.com")))
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

(setq gnus-select-method 
      '(nnmaildir "GMail" 
		  (directory "~/Documents/mail/")
		  (directory-files nnheader-directory-files-safe) 
		  (get-new-mail nil)))

(load "tls")
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      starttls-gnutls-program "/opt/local/bin/gnutls-cli"
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

(define-key gnus-group-mode-map (kbd "vo")
  '(lambda ()
     (interactive)
     (shell-command "offlineimap&" "*offlineimap*" nil)))

(require 'smtpmail)
(add-hook 'mail-mode-hook 'mail-abbrevs-setup)
(setq message-kill-buffer-on-exit t)

(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)

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

(defun mail-notify ()
  (let ((buffer (get-buffer "*Group*"))
        (count 0))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward "\\([[:digit:]]+\\): INBOX" nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    (if (> count 0)
	(shell-command 
	 "/usr/bin/afplay ~/Downloads/Mail_Mother_Fucker.mp3"))))

(add-hook 'gnus-after-getting-new-news-hook 'mail-notify)

(gnus-demon-add-handler 'gnus-group-get-new-news 1 t)
(gnus-demon-add-handler 'gnus-group-save-newsrc 1 t)
(gnus-demon-init)
