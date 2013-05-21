;;; an collection of functions that I developed to execute some commands
;;; asynchronously
;;; only run ob unix-based systems

;;; TODO: stick the output window with the result buffer
;;; using dedicated window
;;; TODO: shortcut keys for close the result window
;;; TODO: check process exit status, if not success, not close the result window
;;; TODO: undo function

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; create new window and close window function
(defvar tmtxt/dired-async-post-process-window-show-time
  "5" "The time to show the result window after the dired async process finish execution, measured in second. This is a string, so if you change this value, please set it as a string.")

(defvar tmtxt/dired-async-result-window-height
  10 "The height of the result window for the dired async process, measured by the number of lines. This is a number, so if you change this value, please set it as a number.")

(defvar tmtxt/dired-async-buffer-list
  () "The list of all current dired async buffers. Do not set this variable manually.")

(defun tmtxt/dired-async-new-async-window ()
  "Create a new window for displaying tmtxt/dired-async process and switch to that window"
  (let ((dired-async-window-height (- (window-total-height (frame-root-window))
									  (+ tmtxt/dired-async-result-window-height 1))))
	(let ((dired-async-window
		   (split-window (frame-root-window) dired-async-window-height 'below)))
	  ;; not allow other-window
	  (set-window-parameter dired-async-window 'no-other-window t)
	  ;; return the new window
	  dired-async-window)))

(defun tmtxt/dired-async-close-window (process)
  "Close the window that contain the process"
  (let ((current-async-buffer (process-buffer process)))
	(let ((current-async-window (get-buffer-window current-async-buffer)))
	  (print
	   (concat
		"Process completed.\nThe window will be closed automatically in "
		tmtxt/dired-async-post-process-window-show-time
		" seconds.")
	   current-async-buffer)
	  ;; remove the the buffer from the buffer list
	  (setq tmtxt/dired-async-buffer-list
	  		(remove (buffer-name current-async-buffer) tmtxt/dired-async-buffer-list))
	  (set-window-point current-async-window
						(buffer-size current-async-buffer))
	  ;; kill the buffer and window after 5 seconds
	  (run-at-time (concat tmtxt/dired-async-post-process-window-show-time " sec")
				   nil 'delete-window current-async-window)
	  (run-at-time (concat tmtxt/dired-async-post-process-window-show-time " sec")
				   nil 'kill-buffer current-async-buffer))))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; argument for command
(defun tmtxt/dired-async-argument
  (cond-variable argument-string)
  "Check if the cond-variable is non-nil, return the argument string"
  (when (not (equal cond-variable nil))
	argument-string))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; main function for executing async command
(defun tmtxt/dired-async (dired-async-command
						  dired-async-command-name
						  dired-async-handler-function)
  "Execute the async shell command in dired.
	dired-async-command: the command to execute
	dired-async-command-name: just the name for the output buffer
	dired-async-handler-function: the function for handling process

	Create a new window at the bottom, execute the dired-async-command and print
	the output the that window. After finish execution, print the message to that
	window and close it after 5s"
  (let ((dired-async-window-before-sync (selected-window))
		(dired-async-output-buffer
		 (concat "*" dired-async-command-name "*" " at " (current-time-string))))

	;; make a new window
	(select-window (tmtxt/dired-async-new-async-window))
	;; not allow popup
	(add-to-list 'same-window-buffer-names dired-async-output-buffer)
	;; run async command
	(async-shell-command dired-async-command dired-async-output-buffer)
	;; set event handler for the async process
	(set-process-sentinel (get-buffer-process dired-async-output-buffer)
						  dired-async-handler-function)
	;; add the new async buffer to the buffer list
	(add-to-list 'tmtxt/dired-async-buffer-list dired-async-output-buffer)
	;; switch the the previous window
	(select-window dired-async-window-before-sync)))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; move point to the end
(defun tmtxt/dired-async-move-all-points-to-end ()
  "Move the point of all current async buffers to the end.
	Sometimes the points in async output buffers stop somewhere in the middle of the buffer, not move to the end to track the progress. Activating this function to fix it."
  (interactive)
  (dolist (buffer tmtxt/dired-async-buffer-list)
	(set-window-point (get-buffer-window buffer)
						(buffer-size (get-buffer buffer)))))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Kill all
(defun tmtxt/dired-async-kill-all ()
  "Kill all current dired async processes."
  (interactive)
  ;; kill all async buffer from the buffer list
  (dolist (buffer tmtxt/dired-async-buffer-list)
	;; get the window contains the buffer
	(let ((window (get-buffer-window buffer)))
	  ;; remove process sentinel
	  (set-process-sentinel (get-buffer-process buffer)
							nil)
	  ;; delete the process
	  (delete-process (get-buffer-process buffer))
	  ;; kill the buffer
	  (kill-buffer buffer)
	  ;; delete the window
	  (delete-window window))
	;; empty the list
	(setq tmtxt/dired-async-buffer-list ())))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Async Rsync
(defun tmtxt/dired-async-rsync (dest)
  "Asynchronously copy file using Rsync for dired.
	This function runs only on Unix-based system.
	Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))

  (let ((files (dired-get-marked-files nil current-prefix-arg))
		dired-async-rsync-command)
	;; the rsync command
	(setq dired-async-rsync-command "rsync ")
	;; append the arguments for rsync command
	(setq dired-async-rsync-command
		 (concat dired-async-rsync-command
				 (tmtxt/dired-async-rsync-arguments)))
	;; add all selected file names as arguments to the rsync command
	(dolist (file files)
	  (setq dired-async-rsync-command
			(concat dired-async-rsync-command (shell-quote-argument file) " ")))
	;; append the destination to the rsync command
	(setq dired-async-rsync-command
		  (concat dired-async-rsync-command (shell-quote-argument dest)))
	;; execute the command asynchronously
	(tmtxt/dired-async dired-async-rsync-command "rsync"
					   'tmtxt/dired-async-rsync-process-handler)))

(defun tmtxt/dired-async-rsync-process-handler (process event)
  "Handler for window that displays the async process.

	Usage: After start an tmtxt/dired-async, call this function

	The function will print the message to notify user that the process is
	completed and automatically kill the buffer and window that runs the
	process."

  ;; check if the process status is exit, then kill the buffer and the window
  ;; that contain that process after 5 seconds (for the user to see the output)
  (when (equal (process-status process) 'exit)
	;; get the current async buffer and window
	(tmtxt/dired-async-close-window process)))

;;; some support functions for async rsync
(defun tmtxt/dired-async-rsync-arguments ()
  "Return the arguments list for rsync command"
  (let (argument-string)
	(setq argument-string
		  (concat (tmtxt/dired-async-rsync-delete-argument)
				  (tmtxt/dired-async-rsync-progress-argument)
				  (tmtxt/dired-async-rsync-verbose-argument)
				  (tmtxt/dired-async-rsync-archive-argument)
				  (tmtxt/dired-async-rsync-compress-argument)))))

(defvar tmtxt/dired-async-rsync-delete-method
  "--delete-during" "Deletion method for dired async rsync delete. Its values can be
--delete-before
--delete-during
--delete-after")
(defvar tmtxt/dired-async-rsync-allow-delete
  nil "Allow dired async rsync to delete.
Do not set this variable manually.")
(defun tmtxt/dired-async-rsync-delete-argument ()
  "Return the delete argument for rsync command"
  (when (equal tmtxt/dired-async-rsync-allow-delete t)
	  (let (delete-option)
		(setq
		 delete-option
		 (concat "--delete "
				 (cond ((equal
						 tmtxt/dired-async-rsync-delete-method
						 "--delete-during")
						tmtxt/dired-async-rsync-delete-method)
					   ((equal
						 tmtxt/dired-async-rsync-delete-method
						 "--delete-after")
						tmtxt/dired-async-rsync-delete-method)
					   (t "--delete-before"))
				 " ")))))

(defvar tmtxt/dired-async-rsync-show-progress
  t "If non-nil, show the progress of rsync process.")
(defun tmtxt/dired-async-rsync-progress-argument ()
  "Return the progress argument for rsync command"
  (tmtxt/dired-async-argument
   tmtxt/dired-async-rsync-show-progress
   "--progress "))

(defvar tmtxt/dired-async-rsync-show-verbosity
  t "If non-nil, run rsync in verbose mode.")
(defun tmtxt/dired-async-rsync-verbose-argument ()
  "Return the progress argument for rsync command"
  (tmtxt/dired-async-argument
   tmtxt/dired-async-rsync-show-verbosity
   "--verbose "))

(defvar tmtxt/dired-async-rsync-archive-mode
  t "If non-nil, use archive mode for rsync.")
(defun tmtxt/dired-async-rsync-archive-argument ()
  "Return the progress argument for rsync command"
  (tmtxt/dired-async-argument
   tmtxt/dired-async-rsync-archive-mode
   "--archive "))

(defvar tmtxt/dired-async-rsync-compress-mode
  t "If non-nil, use compression for rsync")
(defun tmtxt/dired-async-rsync-compress-argument ()
  "Return the progress argument for rsync command"
  (tmtxt/dired-async-argument
   tmtxt/dired-async-rsync-compress-mode
   "--compress "))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Rsync with delete option function
(defun tmtxt/dired-async-rsync-delete (dest)
  "Asynchronously copy file using Rsync for dired include the delete option
	This function runs only on Unix-based system.
	Usage: same as normal dired copy function."
  (interactive ;; offer dwim target as the suggestion
   (list (expand-file-name (read-file-name "Rsync delete to:" (dired-dwim-target-directory)))))
  ;; allow delete
  (setq tmtxt/dired-async-rsync-allow-delete t)
  ;; call the rsync function
  (tmtxt/dired-async-rsync dest)
  ;; reset the delete option
  (setq tmtxt/dired-async-rsync-allow-delete nil))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; async zip files
(defvar tmtxt/dired-async-zip-compression-level
  "9" "The compression level for dired async zip command, from 0-9. This variable is a string, so if you change this value, please set it as a string.")

(defun tmtxt/dired-async-zip (output)
  "Asynchronously compress marked files to the output file"
  (interactive
   (list (expand-file-name (read-file-name "Add to file: "))))

  (let (dired-async-zip-command
		(files (dired-get-marked-files nil current-prefix-arg)))
	;; the zip command
	(setq dired-async-zip-command
		  (concat "zip -ru" tmtxt/dired-async-zip-compression-level " "))
	;; append the output file
	(setq dired-async-zip-command
		  (concat dired-async-zip-command (shell-quote-argument output) " "))
	;; add all selected files as argument
	(dolist (file files)
	  (setq dired-async-zip-command
			(concat dired-async-zip-command
					(shell-quote-argument
					 (file-name-nondirectory file)) " ")))
	;; execute the command asynchronously
	(tmtxt/dired-async dired-async-zip-command "zip"
					   'tmtxt/dired-async-zip-process-handler)))

(defun tmtxt/dired-async-zip-process-handler (process event)
  "Handler for window that displays the zip process.

	Usage: Pass it as an argument when calling tmtxt/dired-async

	The function will print the message to notify user that the process is
	completed and automatically kill the buffer and window that runs the
	process."

  ;; check if the process status is exit, then kill the buffer and the window
  ;; that contain that process after 5 seconds (for the user to see the output)
  (when (equal (process-status process) 'exit)
	;; get the current async buffer and window
	(tmtxt/dired-async-close-window process)))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Uncompress function
(defun tmtxt/dired-async-unzip ()
  "Asynchronously decompress the zip file at point"
  (interactive)

  (let (dired-async-unzip-command
		dired-async-unzip-output-directory
		(file (dired-get-filename 'verbatim)))

	;; new directory name for the output files
	(setq dired-async-unzip-output-directory
		  (file-name-sans-extension
		   (dired-get-filename 'verbatim)))

	;; the unzip command
	(setq dired-async-unzip-command "unzip ")
	;; append the file name
	(setq dired-async-unzip-command
		  (concat dired-async-unzip-command
				  (shell-quote-argument file) " "))
	;; append the output directory name
	(setq dired-async-unzip-command
		  (concat dired-async-unzip-command "-d "
				  (shell-quote-argument dired-async-unzip-output-directory)))

	;; execute the command asynchronously
	(tmtxt/dired-async dired-async-unzip-command "unzip"
					   'tmtxt/dired-async-unzip-process-handler)))

(defun tmtxt/dired-async-unzip-process-handler (process event)
  "Handler for window that displays the zip process.

	Usage: Pass it as an argument when calling tmtxt/dired-async

	The function will print the message to notify user that the process is
	completed and automatically kill the buffer and window that runs the
	process."

  ;; check if the process status is exit, then kill the buffer and the window
  ;; that contain that process after 5 seconds (for the user to see the output)
  (when (equal (process-status process) 'exit)
	;; get the current async buffer and window
	(tmtxt/dired-async-close-window process)))

;;; ----------------------------------------------
;;; ----------------------------------------------
;;; Rsync from multiple directories
(defvar tmtxt/dired-async-rsync-multiple-file-list
  () "The list of the files to be copied")

(defun tmtxt/dired-async-rsync-multiple-mark-file ()
  "Add file to waiting list for copying"
  (interactive)
  ;; Add file to the list
  (add-to-list 'tmtxt/dired-async-rsync-multiple-file-list
			   (dired-get-filename))
  ;; Message for user
  (message
   (concat "File " (dired-get-filename 'verbatim) " added to waiting list.")))

(defun tmtxt/dired-async-rsync-multiple-empty-list ()
  "Empty the waiting list"
  (interactive)
  ;; Empty the list
  (setq tmtxt/dired-async-rsync-multiple-file-list '())
  ;; message for the user
  (message "Waiting list empty."))

(defun tmtxt/dired-async-rsync-multiple-remove-item ()
  "Remove the file at point from the waiting list if it is in"
  (interactive)
  (let ((file-to-remove (dired-get-filename)))
	;; remove the item from the list
	(setq tmtxt/dired-async-rsync-multiple-file-list
		  (remove file-to-remove tmtxt/dired-async-rsync-multiple-file-list))
	;; message for the use
	(message
	 (concat "File " (dired-get-filename 'verbatim) " removed from the list."))))

;; Copy file from multiple directories
(defun tmtxt/dired-async-rsync-multiple ()
  "Mark file in multiple places and then paste in 1 directory"
  (interactive)

  (let (dired-async-rsync-multiple-command)
	(if (equal tmtxt/dired-async-rsync-multiple-file-list ())
		(progn
		  (message "Please add file to the waiting list."))
	  (progn
		;; the rsync command
		(setq dired-async-rsync-multiple-command "rsync -arvz --progress ")
		;; add all selected file names as arguments to the rsync command
		(dolist (file tmtxt/dired-async-rsync-multiple-file-list)
		  (setq dired-async-rsync-multiple-command
				(concat dired-async-rsync-multiple-command (shell-quote-argument file) " ")))
		;; append the destination to the rsync command
		(setq dired-async-rsync-multiple-command
			  (concat dired-async-rsync-multiple-command
					  (shell-quote-argument (expand-file-name default-directory))))
		;; execute the command asynchronously
		(tmtxt/dired-async dired-async-rsync-multiple-command "rsync"
						   'tmtxt/dired-async-rsync-process-handler)
		;; empty the waiting list
		(tmtxt/dired-async-rsync-multiple-empty-list)))))

(provide 'tmtxt-dired-async)
