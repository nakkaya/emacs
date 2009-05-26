;; Author: Nurullah Akkaya (nurullah@nakkaya.com)
;; Location: http://nakkaya.com
;; Version: 1.0

(defvar dsniff-network-interface "ath0"
  "network interface used by ifconfig")
(defvar dsniff-sysctl-program "sysctl")
(defvar dsniff-route-program "route")
(defvar dsniff-arpspoof-program "arpspoof")
(defvar dsniff-dsniff-program "dsniff")
(defvar dsniff-urlsnarf-program "urlsnarf")
(defvar dsniff-msgsnarf-program "msgsnarf")
(defvar dsniff-mailsnarf-program "mailsnarf")
(defvar dsniff-tcpdump-program "tcpdump")
(defvar dsniff-os "linux")
(defvar dsniff-ip-forward nil)
(defvar dsniff-target-host "0.0.0.0")
(defvar dsniff-target-gateway "0.0.0.0")
(defvar dsniff-target-process-host)
(defvar dsniff-target-process-gateway)
(defvar dsniff-sniffing nil )
(defvar dsniff-urlsnarf-running nil)
(defvar dsniff-dsniff-running nil)
(defvar dsniff-msgsnarf-running nil)
(defvar dsniff-mailsnarf-running nil)
(defvar dsniff-tcpdump-capture-raw-running nil)

(defvar dsniff-active-button '(:foreground "steelblue") )
(defvar dsniff-quit-face '( :foreground "firebrick" ))

(defun dsniff-clear-buffer ( button )
  (with-current-buffer "*dsniff*"
    (erase-buffer)
    (dsniff-print-header)))

(defun dsniff-start-arpspoof ()
  (setq dsniff-target-process-host
	(sudo-start-process 
	 "arpspoof-host"
	 (list dsniff-arpspoof-program
	       "-i"
	       dsniff-network-interface
	       "-t"
	       dsniff-target-host
	       dsniff-target-gateway)))
  (setq dsniff-target-process-gateway
	(sudo-start-process 
	 "arpspoof-gateway"
	 (list dsniff-arpspoof-program
	       "-i"
	       dsniff-network-interface
	       "-t"
	       dsniff-target-gateway
	       dsniff-target-host)))
  (with-current-buffer "*arpspoof-gateway*"
    (set (make-local-variable 'truncate-lines) t)
    (setq major-mode 'dsniff-mode
	  mode-name "dsniff"))
  (with-current-buffer "*arpspoof-host*"
    (set (make-local-variable 'truncate-lines) t)
    (setq major-mode 'dsniff-mode
	  mode-name "dsniff")))

(defun dsniff-start-sniffing (button)
  (if (eq dsniff-ip-forward nil) 
      (message "turn on ip forward!" )
    (progn
      (if dsniff-sniffing      
	  (progn
	    (dsniff-killall "arpspoof")
	    (setq dsniff-sniffing nil))
	(progn
	  (dsniff-start-arpspoof)
	  (setq dsniff-sniffing t))))
    (dsniff-print-header)))

(defun dsniff-urlsnarf-browse-url (button) 
  (browse-url-at-point))

(defun dsniff-urlsnarf-filter ( process output ) 
  (with-current-buffer "*urlsnarf*"
    (end-of-buffer)
    (setq current-point (point))
    (insert output)
    (goto-char current-point)
    (while (re-search-forward "GET " nil t)
      (setq beg (match-end 0))
      (setq end (- (re-search-forward " " nil t) 1))
      (make-button beg end 'action 
		   'dsniff-urlsnarf-browse-url 'follow-link t)
      (put-text-property beg end 'face dsniff-active-button ))
    (end-of-buffer)))

(defun dsniff-toggle-urlsnarf (button)
  (if dsniff-urlsnarf-running      
      (progn
	(dsniff-killall "urlsnarf")
	(setq dsniff-urlsnarf-running nil))
    (progn
      (set-process-filter
       (sudo-start-process  "urlsnarf"
			    (list dsniff-urlsnarf-program
				  "-i"
				  dsniff-network-interface))
       'dsniff-urlsnarf-filter)
      (with-current-buffer "*urlsnarf*"
	(set (make-local-variable 'truncate-lines) t)
	(setq major-mode 'dsniff-mode
	      mode-name "dsniff"))
      (setq dsniff-urlsnarf-running t)))
  (dsniff-print-header))

(defun dsniff-toggle-tcpdump-capture-raw (button)
  (if dsniff-tcpdump-capture-raw-running      
      (progn
	(dsniff-killall "tcpdump")
	(setq dsniff-tcpdump-capture-raw-running nil))
    (progn
      (sudo-start-process  "tcpdump-raw-ascii"
			   (list dsniff-tcpdump-program
				 "-A"
				 "-s0"
				 "-i"
				 dsniff-network-interface))
      (with-current-buffer "*tcpdump-raw-ascii*"
	(set (make-local-variable 'truncate-lines) t)
	(setq major-mode 'dsniff-mode
	      mode-name "dsniff"))
      (setq dsniff-tcpdump-capture-raw-running t)))
  (dsniff-print-header))

(defun dsniff-toggle-msgsnarf (button)
  (if dsniff-msgsnarf-running      
      (progn
	(dsniff-killall "msgsnarf")
	(setq dsniff-msgsnarf-running nil))
    (progn
      (sudo-start-process  "msgsnarf"
			  (list dsniff-msgsnarf-program
				"-i"
				dsniff-network-interface))
      (with-current-buffer "*msgsnarf*"
	(set (make-local-variable 'truncate-lines) t)
	(setq major-mode 'dsniff-mode
	      mode-name "dsniff"))
      (setq dsniff-msgsnarf-running t)))
  (dsniff-print-header))

(defun dsniff-toggle-mailsnarf (button)
  (if dsniff-mailsnarf-running      
      (progn
	(dsniff-killall "mailsnarf")
	(setq dsniff-mailsnarf-running nil))
    (progn
      (sudo-start-process  "mailsnarf"
			  (list dsniff-mailsnarf-program
				"-i"
				dsniff-network-interface))
      (with-current-buffer "*mailsnarf*"
	(set (make-local-variable 'truncate-lines) t)
	(setq major-mode 'dsniff-mode
	      mode-name "dsniff"))
      (setq dsniff-mailsnarf-running t)))
  (dsniff-print-header))

(defun dsniff-toggle-dsniff (button)
  (if dsniff-dsniff-running      
      (progn
	(dsniff-killall "dsniff")
	(setq dsniff-dsniff-running nil))
    (progn
      (sudo-start-process  "dsniff-p"
			  (list dsniff-dsniff-program
				"-i"
				dsniff-network-interface))
      (with-current-buffer "*dsniff-p*"
	(set (make-local-variable 'truncate-lines) t)
	(setq major-mode 'dsniff-mode
	      mode-name "dsniff"))
      (setq dsniff-dsniff-running t)))
  (dsniff-print-header))

(defun dsniff-killall( name )
  (sudo-start-process "killall" (list "killall" name ))
  (if (get-buffer "*killall*")
      (kill-buffer "*killall*")))

(defun dsniff-set-network-interface ( button )
  (setq dsniff-network-interface (read-string "Interface: "))
  (with-current-buffer "*dsniff*"
    (erase-buffer))
  (dsniff-print-header))

(defun dsniff-set-target-gateway (button)
  (setq dsniff-target-gateway
	 (read-string "Gateway: "))
  (dsniff-clear-buffer button))

(defun dsniff-set-target-host (button)
  (setq dsniff-target-host
	 (read-string "Host: "))
  (dsniff-clear-buffer button))

(defun dsniff-toggle-ip-forward-linux-sentinel(process status)
  (with-current-buffer "*dsniff-process*" 
    (kill-buffer "*dsniff-process*")))

(defun dsniff-toggle-ip-forward ( button)
(if (string= dsniff-os "linux")
    (progn    
      (if dsniff-ip-forward 
	  (progn 
	    (setq dsniff-ip-forward nil)
	    (setq option(concat "net.ipv4.ip_forward=" "0")))
	(progn 
	  (setq dsniff-ip-forward t)
	  (setq option(concat "net.ipv4.ip_forward=" "1"))))))  
(if (string= dsniff-os "osx")
    (progn    
      (if dsniff-ip-forward 
	  (progn 
	    (setq dsniff-ip-forward nil)
	    (setq option(concat "net.inet.ip.forwarding=" "0")))
	(progn 
	  (setq dsniff-ip-forward t)
	  (setq option(concat "net.inet.ip.forwarding=" "1"))))))

(set-process-sentinel    
 (sudo-start-process 
  "dsniff-process"
  (list "sysctl" "-w" option  ) ) 
       'dsniff-toggle-ip-forward-linux-sentinel)
(dsniff-clear-buffer button))

(defun dsniff()
  (interactive)
  (if (eq (get-buffer "*dsniff*") nil)
      (progn
	(dsniff-print-header)
	(dsniff-mode))
    (switch-to-buffer "*dsniff*")))

(defun dsniff-quit ( button )
  (if (get-buffer "*dsniff*")
      (kill-buffer "*dsniff*")))

(defun dsniff-print-header ()
(switch-to-buffer "*dsniff*")
(with-current-buffer "*dsniff*"
  (erase-buffer)
  (setq beg (point))
  (insert "Quit")
  (setq end (point))
  (make-button beg end 'action 'dsniff-quit 'follow-link t)
  (put-text-property beg end 'face dsniff-quit-face )
  (insert "    ")
  (setq beg (point))
  (insert "Clear")
  (setq end (point))
  (make-button beg end 'action 'dsniff-clear-buffer 'follow-link t)
  (put-text-property beg end 'face dsniff-active-button )
  (insert "                                       ")
  (setq beg (point))
  (if dsniff-sniffing
      (insert "[  STOP  ]")
    (insert "[  START  ]"))
  (setq end (point))
  (make-button beg end 'action 'dsniff-start-sniffing 'follow-link t)
  (put-text-property beg end 'face dsniff-quit-face )
  (insert "\n\nInterface:   " )
  (setq beg (point))
  (insert dsniff-network-interface )
  (setq end (point))
  (make-button beg end 'action 'dsniff-set-network-interface 'follow-link t)
  (put-text-property beg end 'face dsniff-active-button )
  (insert "      Ip Forward:  ")
  (setq beg (point))
  (if dsniff-ip-forward
      (insert "ON")
    (insert "OFF"))
  (setq end (point))
  (make-button beg end 'action 'dsniff-toggle-ip-forward 'follow-link t)
  (put-text-property beg end 'face dsniff-active-button )
  (insert "\n\nGateway:                 ")
  (insert "Self:                      ")
  (insert "Host:\n")
;gateway
  (setq beg (point))
  (insert  (format "%-12s" dsniff-target-gateway))
  (setq end (+ beg (length dsniff-target-gateway)))
  (make-button beg end 'action 'dsniff-set-target-gateway 'follow-link t)
  (put-text-property beg end 'face dsniff-active-button )
;self
  (insert "   <------   ")
  (insert (format "%-12s" (nmap-get-ip)))
  (insert "   ------>   ")
  (insert "  ")
;host
  (setq beg (point))
  (insert dsniff-target-host)
  (setq end (point))
  (make-button beg end 'action 'dsniff-set-target-host 'follow-link t)
  (put-text-property beg end 'face dsniff-active-button )
;detect
  (insert "\n")
  (setq beg (point))
  (insert "Detect")
  (setq end (point))
  (make-button beg end 'action 'dsniff-detect-gateway 'follow-link t)
  (put-text-property beg end 'face dsniff-active-button )
  (insert "                                              ")
  (setq beg (point))
  (insert "Nmap import")
  (setq end (point))
  (make-button beg end 'action 'dsniff-nmap-import 'follow-link t)
  (put-text-property beg end 'face dsniff-active-button )
  (insert "\n\n")
  (setq beg (point))
  (insert "dsniff")
  (setq end (point))
  (insert "   [")
  (if dsniff-dsniff-running
      (insert "*")
    (insert " "))
  (insert "]")
  (make-button beg end 'action 'dsniff-toggle-dsniff 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "     ")
  (setq beg (point))
  (insert "urlsnarf")
  (setq end (point))
  (insert "  [")
  (if dsniff-urlsnarf-running
      (insert "*")
    (insert " "))
  (insert "]")
  (make-button beg end 'action 'dsniff-toggle-urlsnarf 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\n")
  (setq beg (point))
  (insert "msgsnarf")
  (setq end (point))
  (insert " [")
  (if dsniff-msgsnarf-running
      (insert "*")
    (insert " "))
  (insert "]")
  (make-button beg end 'action 'dsniff-toggle-msgsnarf 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "     ")
  (setq beg (point))
  (insert "mailsnarf")
  (setq end (point))
  (insert " [")
  (if dsniff-mailsnarf-running
      (insert "*")
    (insert " "))
  (insert "]")
  (make-button beg end 'action 'dsniff-toggle-mailsnarf 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\n\nTcpDump\n\n")
  (setq beg (point))
  (insert "Capture all ASCII")
  (setq end (point))
  (insert " [")
  (if dsniff-tcpdump-capture-raw-running
      (insert "*")
    (insert " "))
  (insert "]")
  (make-button beg end 'action 
	       'dsniff-toggle-tcpdump-capture-raw 'follow-link t)
  (put-text-property beg end 'face nmap-active-button )
  (insert "\n")))

(defun dsniff-nmap-import (button)
  (setq dsniff-nmap-list (list))
  (with-current-buffer "*nmap*"
    (beginning-of-buffer)
    (while (re-search-forward 
	    "[0-9]+\\.[0-9]*\\.[0-9]*\\.[0-9]*" nil t)
      (add-to-list 'dsniff-nmap-list 
		   (buffer-substring-no-properties 
		    (match-beginning 0) (match-end 0) ))))
  (with-current-buffer "*dsniff*"
    (end-of-buffer)
      (insert "\n\n")
      (insert "Pick  Host:  ")
      (dolist (host dsniff-nmap-list)
	(setq beg (point))
	(insert (format "%-12s" host ) )
	(setq end (+ beg (length host)))
	(insert	"\n             ")
	(make-button beg end 'action 'dsniff-nmap-set-host 'follow-link t)
	(put-text-property beg end 'face dsniff-active-button ))))

(defun dsniff-nmap-set-host ( button )
  (setq dsniff-target-host 
	      (buffer-substring-no-properties 
	       (button-start button ) (button-end button )))
  (dsniff-clear-buffer button))

(defun dsniff-detect-gateway(button)
(with-temp-buffer
	  (setq route-command 
		(concat dsniff-route-program " get" " default"))
	  (shell-command route-command 1 )
	  (if (search-forward "gateway" nil t)
	      (if (re-search-forward 
		   "[0-9]*\\.[0-9]*\\.[0-9]*\\.[0-9]*" nil t)
		  (setq dsniff-target-gateway 
			(buffer-substring (match-beginning 0) (point) ) ))
	    (setq dsniff-target-gateway "0.0.0.0"  )))
(dsniff-clear-buffer button))

(defun dsniff-mode ()
  "dsniff controling mode."
  (set (make-local-variable 'truncate-lines) t)
  (setq major-mode 'dsniff-mode
	mode-name "dsniff"))
(provide 'dsniff)
