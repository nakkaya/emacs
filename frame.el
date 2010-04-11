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

(set-face-background 'org-hide "#2A2A38")
(set-face-foreground 'org-hide "#2A2A38")

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
