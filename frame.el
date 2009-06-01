;;emacs theme
(setq frame-title-format (list "GNU Emacs " emacs-version))
;;show current cpu load
(setq display-time-day-and-date nil )
(setq display-time-format "") 
(setq display-time-load-average-threshold 0 )
(setq display-time-string-forms '( load "," (if mail "" "")) )
(setq display-time-interval 5)
(display-time-mode 1)
;(display-time-mode 0)
;;display battery
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
;		user-login-name "@" ;;  you@
		"%f"		    ;; print file with full path
		" %-"))
;; Here are some color preferences.  I've spent a good deal of time
;; adjusting color settings, trying to get it to look good and also be
;; fairly easy to read.
(setq default-frame-alist
      (append default-frame-alist
	      ;;'((foreground-color . "yellow")
              '((foreground-color . "forest green")
		;; '((foreground-color . "wheat")
                (background-color . "black")
		;; (cursor-color . "green3"))))
                (cursor-color . "blue"))))

(set-face-foreground 'bold "forest green")
(set-face-background 'bold "black")

;(set-face-foreground 'default "forest green")
(set-face-background 'default "black")

(set-face-foreground 'bold-italic "yellow green")
(set-face-foreground 'italic "yellow3")

(set-face-foreground 'region "white")
(set-face-background 'region "blue")

(set-face-foreground 'modeline "black")
(set-face-background 'modeline "forest green")

(set-face-foreground 'modeline-inactive "dark green")
(set-face-background 'modeline-inactive "black")

(set-face-foreground 'diff-added "blue")
(set-face-background 'diff-added "black")

(set-face-foreground 'diff-removed "firebrick")
(set-face-background 'diff-removed "black")

(set-face-foreground 'diff-file-header "blue")
(set-face-background 'diff-file-header "black")

(set-face-foreground 'diff-header "forest-green")
(set-face-background 'diff-header "black")

;;gnus theme
;;;; Faces ;;;;
;;;
;; Group level faces.
(defun my-gnus-article-prepare-hook-fn ()
  "Function added to `gnus-article-prepare-hook'."
  (require 'gnus-cite)                  ; NB why?
  ;; "X wrote:" font.
  (face-spec-set 'gnus-cite-attribution-face
                 '((t (:slant italic :family "helv"
                              :foreground "SkyBlue"))))
  (face-spec-set 'gnus-cite-face-1
                 '((((type x)) (:foreground "light blue"))
                   (t (:foreground "turquoise"))))

(face-spec-set 'gnus-cite-attribution '((t (:foreground "DarkSlateGray"))))
(face-spec-set 'gnus-cite-1 '((t (:foreground "chartreuse4"))))
(face-spec-set 'gnus-cite-2 '((t (:foreground "SeaGreen"))))
(face-spec-set 'gnus-cite-3 '((t (:foreground "forest green"))))
(face-spec-set 'gnus-cite-4 '((t (:foreground "DarkSlateGray1"))))
(face-spec-set 'gnus-cite-5 '((t (:foreground "spring green"))))
(face-spec-set 'gnus-cite-6 '((t (:foreground "green")))))
(add-hook 'gnus-article-prepare-hook 'my-gnus-article-prepare-hook-fn)

(defun my-gnus-article-mode-hook-fn ()
  "Function added to `gnus-article-mode-hook'."
  ;; Misc faces.
  (face-spec-set 'gnus-emphasis-bold-italic
                 '((t (:slant italic :family "helv" :weight bold))))

  (face-spec-set 'gnus-emphasis-italic
                 '((t (:slant italic :family "helv"))))

  (face-spec-set 'gnus-emphasis-strikethru '((t (:strike-through t))))

  (face-spec-set 'gnus-emphasis-underline-bold-italic
                 '((t (:slant italic :family "helv" :weight bold
                              :underline t))))

  (face-spec-set 'gnus-emphasis-underline-italic
                 '((t (:slant italic :family "helv" :underline t))))

  ;; Article header faces.
  (face-spec-set 'gnus-header-name-face '((t (:foreground "SeaGreen"))))

  (face-spec-set 'gnus-header-from-face '((t (:foreground "spring green"))))

  (face-spec-set 'gnus-header-newsgroups-face '((t (:foreground "yellow"))))

  (face-spec-set 'gnus-header-subject-face '((t (:foreground "SeaGreen3"))))

  (face-spec-set 'gnus-header-content-face '((t (:foreground "forest green"))))

  (face-spec-set 'gnus-signature-face '((t (:foreground "darkslategray")))))

(add-hook 'gnus-article-mode-hook 'my-gnus-article-mode-hook-fn)


;;group faces
(face-spec-set 'gnus-group-mail-1 '((t (:foreground "blue1"))))
(face-spec-set 'gnus-group-news-2 '((t (:foreground "darkgreen"))))
(face-spec-set 'gnus-group-news-3 '((t (:foreground "darkgreen"))))

(face-spec-set 'gnus-group-mail-1-empty '((t (:foreground "royalblue"))))
(face-spec-set 'gnus-group-news-2-empty '((t (:foreground "royalblue"))))
(face-spec-set 'gnus-group-news-3-empty '((t (:foreground "royalblue"))))

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


;; Eg those marked down by scoring, so should be inconspicuous.
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
			   (summary 1.0 point)
			   )
		 (vertical 1.0
			   (article 1.0)
			   ))))
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
		 (vertical 1.0
			   ))))
  (gnus-add-configuration
   '(forward
     (horizontal 1.0
		 (vertical 0.50
			   ( article 1.0 ) )
		 (vertical 1.0
			   ))))
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


(defun na-resize-frame-big ()
  "Set size"
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 130 :width 'normal)
  (set-frame-width (selected-frame) 178)
  (set-frame-height (selected-frame) 55 )
  (set-frame-position (selected-frame) 0 1)) 

(defun na-resize-frame-big-bf ()
  "Set size"
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 150 :width 'normal)  
  (set-frame-width (selected-frame) 158 )
  (set-frame-height (selected-frame) 43 )
  (set-frame-position (selected-frame) 0 1))

(defun na-resize-frame-small ()
  "Set size"
  (interactive)
  (set-frame-width (selected-frame) 77)
  (set-frame-height (selected-frame) 40))

(defun na-resize-frame-bizdik ()
  "Set size"
  (interactive)
;  (set-default-font "monaco")
  (set-face-attribute 'default (selected-frame) :height 110 :width 'normal)
  (set-frame-width (selected-frame) 110)
  (set-frame-height (selected-frame) 33)
  (set-frame-position (selected-frame) 0 1))

(defun na-resize-frame-bizdik-bf ()
  "Set size"
  (interactive)
  (set-face-attribute 'default (selected-frame) :height 130 :width 'normal)
  (set-frame-width (selected-frame) 70 )
  (set-frame-height (selected-frame) 22 )
  (set-frame-position (selected-frame) 0 1))

(defun na-frame-windows ()
  "Set size"
  (interactive)
  (set-default-font "monaco")
  (set-face-attribute 'default (selected-frame) :height 100 :width 'normal)
   (set-frame-width (selected-frame) 130)
   (set-frame-height (selected-frame) 50)
   (set-frame-position (selected-frame) 0 1))
