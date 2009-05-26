;; Author: Nurullah Akkaya (nurullah@nakkaya.com)
;; Location: http://nakkaya.com
;; Version: 1.0

(load-library "sudo")

(defvar nmap-nmap-program "/opt/local/bin/nmap"
  "nmap command")
(defvar nmap-ifconfig-program "ifconfig"
  "ifconfig command")
(defvar nmap-network-interface "en1"
  "network interface used by ifconfig")
(defvar nmap-output-buffer "*nmap-process*"
  "The user's cached sudo password, or an empty string.")
(setq nmap-port-scan-options (list "-n" ))
(defvar nmap-detect-os nil
  "Whether to os detection")
(defvar nmap-use-decoy nil 
  "Whether to use decoys while scanning" )
(defvar nmap-decoy-list "" 
  "Decoy list random ip's created from the same ip block")
(defvar nmap-number-of-decoys 4 
  "Number of decoys")
(defvar nmap-detect-service nil 
  "Whether to do service/info detection")
(defvar nmap-use-timing nil)
(defvar nmap-time-template 5)
(defvar nmap-targets "0.0.0.0")
;;(defvar nmap-online-targets "")
(defvar nmap-process-stack 0 
  "Number of running nmap processes")

(defvar nmap-mode-map nil)
(setq nmap-mode-map (make-keymap))
(define-key nmap-mode-map "c" 'nmap-mode-map-clear)
(define-key nmap-mode-map "t" 'nmap-mode-map-target)
(define-key nmap-mode-map "h" 'nmap-mode-map-scan-host)
(define-key nmap-mode-map "s" 'nmap-mode-map-scan-targets)
(define-key nmap-mode-map "q" 'nmap-mode-map-quit)

(defun nmap-mode-map-quit( arg )
(interactive "p")
  (nmap-quit ""))

(defun nmap-mode-map-clear( arg )
(interactive "p")
  (nmap-clear-buffer ""))

(defun nmap-mode-map-target( arg )
(interactive "p")
  (nmap-set-targets ""))

(defun nmap-mode-map-scan-host( arg )
(interactive "p")
  (nmap-host-discovery ""))

(defun nmap-mode-map-scan-targets( arg )
(interactive "p")
  (nmap-app-fingerprint ""))

(defvar nmap-active-button '(:foreground "steelblue") )
(defvar nmap-highligth-host-face '( :foreground "blue") )
(defvar nmap-options-face '( :foreground "deepskyblue3"))
(defvar nmap-quit-face '( :foreground "firebrick" ))

(defun nmap-change-interface ( button )
  (setq nmap-network-interface (read-string "Interface: "))
  (with-current-buffer "*nmap*"
    (erase-buffer))
  (nmap-print-header))

(defun nmap-get-ip ()
(interactive)
	(with-temp-buffer
	  (setq ifconfig-command (concat nmap-ifconfig-program 
					 " "
					 nmap-network-interface))
	  (shell-command ifconfig-command 1 )

	  (if (search-forward "inet" nil t)
	      (if (re-search-forward 
		   "[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*" nil t)
		  (buffer-substring (match-beginning 0) (point) ) )
	    "0.0.0.0"   )))

(defun nmap-get-ip-block ()
  (interactive)
  (setq nmap-ip-block
	;;get ip block
	(with-temp-buffer
	  (insert (nmap-get-ip))
	  (beginning-of-buffer)
	  (re-search-forward "[0-9]*\\.[0-9]*\\.[0-9]*\\.")
	  (buffer-substring (point-min) (point) ))))

(defun nmap-host-discovery ( button )
  (interactive)  
  (setq block (concat nmap-ip-block "1-255" ) )  
  (setq options (list "-sP" 
		      "-PS21,22,23,25,80,135,139,445,1025,3389"
		      "-PU53,67,68,69,111,161,445,514"
		      "-PE" "-PP" "-PM"
		      block
		      ))
  (if (not(string= nmap-decoy-list ""))
      (add-to-list 'options nmap-decoy-list ) )

  (if (not(string= nmap-use-timing ""))
      (progn
	(setq timing (concat "-T" (number-to-string nmap-time-template) ))
	(add-to-list 'options  timing ) ))
  (nmap-print-options options)  
  ;;run nmap
  (setq nmap-process-stack (+ nmap-process-stack 1 ))
  (set-process-sentinel 
   (sudo-start-process 
    "nmap-process"
    (cons nmap-nmap-program options) ) 'nmap-process-sentinel))

(defun nmap-app-fingerprint ( button )
  ;;grap burrent targets
  (if (not(string= nmap-targets "0.0.0.0"))
      (setq ip nmap-targets)
    (setq ip (buffer-substring-no-properties 
	      (button-start button ) (button-end button ))))
  ;; (if (string= ip "Scan All Found Host")
  ;;     (setq ip nmap-online-targets ))

  (end-of-buffer)

  ;;reset options
  (setq options (list  ))
  ;;split host list
  (dolist (ips (split-string ip)) 
    (add-to-list 'options ips))

  (dolist (option nmap-port-scan-options)
    (add-to-list 'options option))
  (if nmap-detect-service
      (add-to-list 'options "-sV" ))
  (if (eq nmap-use-timing t)
      (progn
	(setq timing (concat "-T" (number-to-string nmap-time-template) ))
	(add-to-list 'options  timing ) ))
  (nmap-print-options options)
  ;;run nmap
  (setq nmap-process-stack (+ nmap-process-stack 1 ))
  (set-process-sentinel 
   (sudo-start-process 
    "nmap-process"
    (cons nmap-nmap-program options) ) 'nmap-process-sentinel) )

(defun nmap-set-timing ( button )
 (setq nmap-time-template (string-to-number (read-string "T: ")))  
  (nmap-clear-buffer button))

(defun nmap-set-number-decoy ( button )
  (setq nmap-number-of-decoys 
	(string-to-number (read-string "Number of decoys: ")))  
  (nmap-clear-buffer button))

(defun nmap-toggle-os-detection ( button )
  (setq option "-O")
  (if nmap-detect-os      
    (progn
      (setq nmap-port-scan-options 
	    (delete  option nmap-port-scan-options ))
      (setq nmap-detect-os nil))
      (progn
	(add-to-list 'nmap-port-scan-options option)
	(setq nmap-detect-os t)))
  (nmap-clear-buffer button))

(defun nmap-toggle-use-decoys (button)
  (with-temp-buffer
    (insert "-D")
    (setq counter 1)
    (insert (nmap-get-ip-block) (number-to-string (random 255) ))
    (while (< counter nmap-number-of-decoys)
      (insert "," )
      (insert (nmap-get-ip-block) (number-to-string (random 255) ))
      (incf counter))
    (setq nmap-decoy-list (buffer-string)))
  (if nmap-use-decoy
      (progn
	(setq nmap-decoy-list "")
	(setq nmap-use-decoy nil))
    (setq nmap-use-decoy t))
  (nmap-clear-buffer button))

(defun nmap-toggle-use-timing (button)
(if nmap-use-timing
    (setq nmap-use-timing nil)
  (setq nmap-use-timing t))
(nmap-clear-buffer button))

(defun nmap-toggle-detect-service (button)
  (if nmap-detect-service
      (setq nmap-detect-service nil)
    (setq nmap-detect-service t))
  (nmap-clear-buffer button))

(defun nmap-set-targets (button)
  (setq nmap-targets (read-string "Target(s):"  ))
  (nmap-clear-buffer button))

(defun nmap-print-options ( print-options )
(with-current-buffer "*nmap*"
  (end-of-buffer)
  (insert "\nScanning...\n")
  (insert "options: ")
  (dolist (option print-options)
    (setq beg (point))
    (insert option )
    (setq end (point))
    (insert "\n\t ")
    (put-text-property beg end 'face nmap-options-face ))
  (insert "\n")))

(defun nmap-quit ( button )
  (if (get-buffer "*nmap-process*")
      (kill-buffer "*nmap-process*"))
(if (get-buffer "*nmap*")
      (kill-buffer "*nmap*")))

(defun nmap-print-header()
  (switch-to-buffer "*nmap*")
  (erase-buffer)

  (setq beg (point))
  (insert "Quit")
  (setq end (point))
  (make-button beg end 'action 'nmap-quit 'follow-link t)
  (put-text-property beg end 'face nmap-quit-face )
  (insert "    ")
  (setq beg (point))
  (insert "Clear")
  (setq end (point))
  (make-button beg end 'action 'nmap-clear-buffer 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\n\nTarget(s):  " )
  (setq beg (point))
  (insert nmap-targets )
  (setq end (point))
  (make-button beg end 'action 'nmap-set-targets 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\nInterface:  " )
  (setq beg (point))
  (insert nmap-network-interface )
  (setq end (point))
  (make-button beg end 'action 'nmap-change-interface 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\nIp:         " (nmap-get-ip) "\nIp Block:   " 
	  (nmap-get-ip-block) "1-255")
  
  (insert "\n\n")
  (setq beg (point))
  (insert "OS Detection")
  (setq end (point))
  (insert " [")
  (if nmap-detect-os
      (insert "*")
    (insert " "))
  (insert "]")  
  (make-button beg end 'action 'nmap-toggle-os-detection 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "            ")
  (setq beg (point))
  (insert "Service/Version Info")  
  (setq end (point))
  (insert " [")
  (if nmap-detect-service
      (insert "*")
    (insert " "))
  (insert "]")
  (make-button beg end 'action 'nmap-toggle-detect-service 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\n")
  (setq beg (point))
  (insert "Use Decoys")
  (setq end (point))
  (make-button beg end 'action 'nmap-toggle-use-decoys 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "   [")
  (if nmap-use-decoy
      (insert "*")
    (insert " "))
  (insert "] ")
  (setq beg (point))
  (insert "[" (number-to-string nmap-number-of-decoys) "]" )
  (setq end (point))
  (make-button beg end 'action 'nmap-set-number-decoy 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )  
  (insert "        ")
  (setq beg (point))
  (insert "Timing Template")
  (setq end (point))
  (make-button beg end 'action 'nmap-toggle-use-timing 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "      [")
  (if nmap-use-timing
      (insert "*")
    (insert " "))
  (insert "] ")
  (setq beg (point))
  (insert"[" (number-to-string nmap-time-template) "]" )
  (setq end (point))
  (make-button beg end 'action 'nmap-set-timing 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )  
  (insert "\n\n")
  (setq beg (point))
  (insert "Scan Target(s)")
  (setq end (point))
  (make-button beg end 'action 'nmap-app-fingerprint 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "              ")
  (setq beg (point))
  (insert "Host Discovery" )
  (setq end (point))
  (make-button beg end 'action 'nmap-host-discovery 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\n\n"))

(defun nmap ()
  (interactive)
  (if (eq (get-buffer "*nmap*") nil)
      (progn
	(nmap-print-header)
	(nmap-mode))
    (switch-to-buffer "*nmap*")))

(defun nmap-process-sentinel (process status)
(setq nmap-process-stack (- nmap-process-stack 1 ))
  (with-current-buffer "*nmap*"
    (end-of-buffer)
    (setq port-scan (nmap-parse-port-scan))
    (if port-scan
	(insert port-scan "\n"))
    (setq host-scan (nmap-parse-host-list))
    (if host-scan
	(insert host-scan "\n")))
  (nmap-host-list-highligth)
(if (= nmap-process-stack 0)
    (kill-buffer "*nmap-process*"))
)

(defun nmap-parse-port-scan()
  (interactive)
(with-current-buffer "*nmap-process*"
    (beginning-of-buffer)

    (if (re-search-forward "Start.*" nil t )
	(progn 
	  (setq output-begin (match-beginning 0) )
	  (end-of-buffer)
	  (setq output-end (point))
	  (setq result (buffer-substring output-begin output-end ) )
	  (delete-region output-begin output-end)
	  (concat result "\n")
))))

(defun nmap-parse-host-list()
  (with-current-buffer "*nmap-process*"
    (beginning-of-buffer)
    (if (re-search-forward "^Host " nil t )
	(progn 
	  (setq beg (match-beginning 0) )
	  (if (re-search-forward "Nmap " nil t)
	      (progn
		(setq end (match-beginning 0) )
		(setq result (buffer-substring beg end ))
		(delete-region beg end )
		result))))))

(defun nmap-host-list-highligth ()
  (interactive)
  (with-current-buffer "*nmap*"
  (beginning-of-buffer)
;;  (setq found-host-list nil)
;;  (setq nmap-online-targets "")

  (setq beg  (re-search-forward "^Host " nil t))
  (while (not (eq beg nil) )
    (if (re-search-forward "[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*" nil t)
	(progn
	  (setq end (match-end 0))
	  ;;mark we found host list
;;	  (setq found-host-list t)
	  ;; ;;add each host found to targets
	  ;; (setq nmap-online-targets 
	  ;; 	(concat nmap-online-targets " " (buffer-substring beg end ) ) )

	  (make-button beg end 'action 'nmap-app-fingerprint 'follow-link t)
	  (put-text-property beg end 
			     'face nmap-highligth-host-face )))
    (setq beg (re-search-forward "Host " nil t )))
  ;; (if (eq found-host-list t )
  ;;     (progn 
  ;; 	;;add found hosts button to the bottom
  ;; 	(end-of-buffer)
  ;; 	(setq beg (point))
  ;; 	(insert "Scan All Found Host")
  ;; 	(setq end (point))
  ;; 	(make-button beg end 'action 'nmap-app-fingerprint 'follow-link t)
  ;; 	(put-text-property beg end 'face nmap-highligth-host-face )
  ;; 	(insert "\n"))
  ;;   )
)
  (end-of-buffer))

(defun nmap-clear-buffer ( button )
  (with-current-buffer "*nmap*"
    (erase-buffer)
    (nmap-print-header)))

(defun nmap-mode ()
  "nmap controling mode."
  (set (make-local-variable 'truncate-lines) t)
  (setq major-mode 'nmap-mode
	mode-name "nmap")
  (use-local-map nmap-mode-map))
(provide 'nmap)
