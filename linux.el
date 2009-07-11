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
(if (not (eq (get-buffer "*terminal*")  nil ) )
    (progn     
      (setq found nil)
      (bury-buffer)
      (setq head (car (buffer-list)))      
      (while  (eq found nil)	
	(set-buffer head)	
	(if (eq major-mode 'term-mode )
	    (setq found t )
	  (progn
	   (bury-buffer)
	   (setq head (car (buffer-list)))))))))

(load "mpg123.el")
(setq mpg123-mpg123-command "/usr/bin/mpg123")
(setq mpg123-need-slider nil)
(setq mpg123-show-help t)

(defun na-switch-to-mpg123 ()
  (interactive)
  (other-window 1)
  (set-window-buffer (selected-window) "*mpg123*"))

(defun na-mpg123-run ()
      "run or switch to mpg123 buffer"
      (interactive)
      
      (if (eq (get-buffer "*mpg123*")  nil) 
	  (mpg123 "~/Documents/playlist") 
	(na-switch-to-mpg123)))

(defun dired-mpg123-add ()
      "add current file to mpg123 buffer"
      (interactive)
      (setq file (dired-get-filename) )
(message file)
      (mpg123-add-new file ))
(define-key dired-mode-map "\M-m"  'dired-mpg123-add)

(defun na-scan-wireless ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (erase-buffer)

  (cond 
   ((string= "linux" my-opsys)
    (shell-command "/sbin/iwlist ath0 scan" 1 ))
   ((string= "osx" my-opsys)
    (shell-command "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -s" 1 ))))

;;nmap mode configuration
(cond
 ((string= "linux" my-opsys)
  (setq nmap-nmap-program "/usr/bin/nmap")
  (setq nmap-ifconfig-program "/sbin/ifconfig")
  (setq nmap-network-interface "ath0")))

(cond
 ((string= "osx" my-opsys)
  (setq dsniff-os "osx")
  (setq dsniff-network-interface "en1")
  (setq dsniff-arpspoof-program "/opt/local/sbin/arpspoof")
  (setq dsniff-urlsnarf-program "/opt/local/sbin/urlsnarf")
  (setq dsniff-dsniff-program "/opt/local/sbin/dsniff")
  (setq dsniff-msgsnarf-program "/opt/local/sbin/msgsnarf")
  (setq dsniff-mailsnarf-program "/opt/local/sbin/mailsnarf")
  (setq dsniff-tcpdump-program "/opt/local/sbin/tcpdump")))

(defun na-proxy-start ()
  (interactive)
  (start-process "ssh socks" "ssh socks" 
		 "ssh" "-ND" "9999" "-p" "5432" "-v" "nakkaya@nakkaya.com")
)

(defun na-proxy-stop ()
  (interactive)
  (quit-process "ssh socks")
)
