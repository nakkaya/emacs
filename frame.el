;;emacs theme
(setq frame-title-format (list "GNU Emacs " emacs-version))
(setq display-time-day-and-date nil )
(setq display-time-format "") 
(setq display-time-load-average-threshold 0 )
(setq display-time-string-forms '( load "," (if mail "" "")) )
(setq display-time-interval 5)
(display-time-mode 1)
(setq battery-mode-line-format "%b%p%" )
(display-battery-mode t)
;;mode line custom
(setq-default mode-line-format
	      '(""
		mode-line-modified
		(-3 . "%p") ;; position
		"[%b]"
		"%[("
		mode-name
		mode-line-process
		minor-mode-alist
		"%n" ")%]-"
		(line-number-mode "L%l-")
		(column-number-mode "C%c [")
		global-mode-string
		"] "
		"%f"		    ;; print file with full path
		" %-"))

;;
;; Theme
;;
;;
;; Emacs
;;

(setq default-frame-alist
      (append default-frame-alist
              '((foreground-color . "#EEEEEC")
                (background-color . "#2A2A38")
                (cursor-color . "#FCE94F"))))

(set-face-foreground 'bold "#EEEEEC")
(set-face-background 'bold "#2A2A38")
(set-face-background 'default "#2A2A38")

(set-face-foreground 'font-lock-string-face "#854BBE")
(set-face-foreground 'font-lock-keyword-face "#A02350")
(set-face-foreground 'font-lock-function-name-face "#C3A878")
(set-face-foreground 'font-lock-builtin-face "#729FCF")
(set-face-foreground 'font-lock-comment-face "#888A85")

(set-face-foreground 'modeline "#A5A5A0")
(set-face-background 'modeline "#555753")
(set-face-foreground 'modeline-inactive "#64645F")
(set-face-background 'modeline-inactive "black")

(set-face-background 'fringe "#2A2A38")
(set-face-foreground 'vertical-border "#888A85")

(set-face-foreground 'diff-added "#EEEEEC")
(set-face-foreground 'diff-removed "firebrick")
(set-face-foreground 'diff-file-header "#EEEEEC")
(set-face-foreground 'diff-header "#EEEEEC")

(set-face-background 'diff-header "#2A2A38")
(set-face-background 'diff-file-header "#2A2A38")

;;
;; Gnus
;;

;;group faces
(face-spec-set 'gnus-group-mail-1 '((t (:foreground "blue1"))))
(face-spec-set 'gnus-group-news-2 '((t (:foreground "darkgreen"))))
(face-spec-set 'gnus-group-news-3 '((t (:foreground "darkgreen"))))

(face-spec-set 'gnus-group-mail-1-empty '((t (:foreground "royalblue"))))
(face-spec-set 'gnus-group-news-2-empty '((t (:foreground "royalblue"))))
(face-spec-set 'gnus-group-news-3-empty '((t (:foreground "royalblue"))))

;;citing faces
(face-spec-set 'gnus-cite-attribution '((t (:foreground "DarkSlateGray"))))
(face-spec-set 'gnus-cite-1 '((t (:foreground "chartreuse4"))))
(face-spec-set 'gnus-cite-2 '((t (:foreground "SeaGreen"))))
(face-spec-set 'gnus-cite-3 '((t (:foreground "forest green"))))
(face-spec-set 'gnus-cite-4 '((t (:foreground "DarkSlateGray1"))))
(face-spec-set 'gnus-cite-5 '((t (:foreground "spring green"))))
(face-spec-set 'gnus-cite-6 '((t (:foreground "green"))))

;; Article header faces.
(face-spec-set 'gnus-header-name-face '((t (:foreground "SeaGreen"))))
(face-spec-set 'gnus-header-from-face '((t (:foreground "spring green"))))
(face-spec-set 'gnus-header-newsgroups-face '((t (:foreground "yellow"))))
(face-spec-set 'gnus-header-subject-face '((t (:foreground "SeaGreen3"))))
(face-spec-set 'gnus-header-content-face '((t (:foreground "forest green"))))
(face-spec-set 'gnus-signature-face '((t (:foreground "darkslategray"))))

;; Summary faces.
(face-spec-set 'gnus-summary-high-unread-face
               '((t (:foreground "firebrick" :weight bold))))
(face-spec-set 'gnus-summary-normal-unread-face
               '((t (:foreground "forest green"))))
(face-spec-set 'gnus-summary-low-unread-face
               '((t (:slant italic :family "helv"))))
(face-spec-set 'gnus-summary-high-read-face
               '((t (:foreground "firebrick" :weight bold))))
(face-spec-set 'gnus-summary-normal-read-face
               '((t (:foreground "steelblue"))))

(set-face-foreground 'gnus-summary-high-ticked-face "firebrick")
(set-face-foreground 'gnus-summary-high-ancient-face "firebrick")

(set-face-foreground 'gnus-summary-cancelled "#888A85")
(set-face-background 'gnus-summary-cancelled "#2A2A38")

;;those marked down by scoring, so should be inconspicuous.
(face-spec-set 'gnus-summary-low-read-face
               '((t (:foreground "steelblue"))))
(face-spec-set 'gnus-summary-normal-ticked-face
               '((t (:foreground "firebrick"))))
(face-spec-set 'gnus-summary-low-ticked-face
               '((t (:foreground "steelblue" :slant italic :family "helv"))))
(face-spec-set 'gnus-summary-normal-ancient-face
               '((t (:foreground "steelblue"))))
(face-spec-set 'gnus-summary-low-ancient-face
               '((t (:foreground "steelblue" :slant italic :family "helv"))))


;; Message faces.
(face-spec-set 'message-header-name-face
               '((t (:foreground "SeaGreen"))))
(face-spec-set 'message-header-to-face
               '((t (:foreground "yellow" :weight bold))))
(face-spec-set 'message-header-cc-face
               '((t (:foreground "yellow"))))
(face-spec-set 'message-header-subject-face
               '((t (:foreground "lawngreen"))))
(face-spec-set 'message-header-other-face
               '((t (:foreground "yellow"))))
(face-spec-set 'message-separator-face
               '((t (:foreground "pink"))))
(face-spec-set 'message-header-newsgroups-face
               '((t (:foreground "yellow" :weight bold))))
(face-spec-set 'message-cited-text-face
               '((t (:foreground "DarkSlateGray1" :weight bold))))

(cond 
 ((string= "linux" my-opsys)
  (gnus-add-configuration 
   '(article (vertical 1.0 (summary .40 point) (article 1.0)))))
 ((string= "osx" my-opsys)	      
  (gnus-add-configuration
   '(article
     (horizontal 1.0
		 (vertical 0.50
			   (summary 1.0 point))
		 (vertical 1.0
			   (article 1.0)))))
  (gnus-add-configuration
   '(summary
     (horizontal 1.0
		 (vertical 0.50
			   (group 1.0))
		 (vertical 1.0
			   (summary 1.0 point)))))
  (gnus-add-configuration
   '(reply
     (horizontal 1.0
		 (vertical 0.50
			   ( article 1.0 ) )
		 (vertical 1.0 ))))
  (gnus-add-configuration
   '(forward
     (horizontal 1.0
		 (vertical 0.50
			   ( article 1.0 ) )
		 (vertical 1.0 ))))
  (gnus-add-configuration
   '(group
     (horizontal 1.0
		 (vertical 0.50
			   (group 1.0 point) )
		 (vertical 1.0
			   (group 1.0) ))))
  (gnus-add-configuration
   '(message
     (horizontal 1.0
		 (vertical 0.50
			   (message 1.0 point) )
		 (vertical 1.0
			   (group 1.0) ))))))

(defun na-set-frame-size(width height font-size)
  (set-face-attribute 
   'default (selected-frame) :height font-size :width 'normal)
  (set-frame-width (selected-frame) width)
  (set-frame-height (selected-frame) height)
  (set-frame-position (selected-frame) 0 1))

(defun na-resize-frame-big ()
  (interactive)  
  (na-set-frame-size 178 55 130))

(defun na-resize-frame-big-bf ()
  (interactive)
  (na-set-frame-size 158 43 150))

(defun na-resize-frame-bizdik ()
  (interactive)
  (na-set-frame-size 110 33 110))

(defun na-resize-frame-bizdik-bf ()
  (interactive)
  (na-set-frame-size 77 22 130))

(defun na-frame-windows ()
  (interactive)
  (set-default-font "monaco")
  (na-set-frame-size 130 50 110))
