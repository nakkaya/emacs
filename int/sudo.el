;; Author: Nurullah Akkaya (nurullah@nakkaya.com)
;; Location: http://nakkaya.com
;; Version: 1.0
;;
;; Shared Code from
;; 
;; sudo.el -- sudo wrapper for saving / opening files
;; Author: Scott Vokes <scott@silentbicycle.com>
;; Version: 0.5 -- 2007-07-14
;;
;;(sudo-start-process "ls" (list "ls"))

(defvar sudo-cached-password ""
  "The user's cached sudo password, or an empty string.")

(defun sudo-start-process ( sudo-process-name command )
(interactive)
(setq sudo-process-buffer-name (concat "*" sudo-process-name  "*"))

(setq sudo-process
(let ((process-connection-type nil)) 
  (apply 'start-process 
	 (append (list sudo-process-name 
		       sudo-process-buffer-name 
		       "sudo" ) command ))))
(sleep-for .25)                      
(if (sudo-wants-password-p sudo-process )
    (sudo-send-sudo-process-password sudo-process))
sudo-process)

(defun sudo-wants-password-p ( sudo-process )
  "Check output buffer to see if sudo is waiting for a password."
  (with-current-buffer (process-buffer sudo-process )
    (goto-char (point-min))
    (if (search-forward "password" nil 1)
        (progn 
	  (erase-buffer)
	  t )
      nil)))

(defun sudo-send-sudo-process-password (sudo-process)
  "Send the sudo process the user's password and an ENTER."
  (process-send-string sudo-process
                       (concat
                        (sudo-get-password)
                        "")))

(defun sudo-get-password ()
  "Prompt for sudo password, caching it if desired."
  (if (string-equal sudo-cached-password "")
      (setq sudo-cached-password
	    (read-passwd "sudo password: "))
    sudo-cached-password))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'sudo)
