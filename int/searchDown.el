;; Author: Nurullah Akkaya (nurullah@nakkaya.com)
;; Location: http://nakkaya.com
;; Version: 1.0

;;
;; This module will traverse a directory tree and
;; search for a regex. Exclude directories and
;; exclude files variables determine which files
;; and directories will be skipped.
;;
;; Installation
;; Load this file from emacs 
;; (load "searchDown.el")
;; and call na-search-down using M-x
;;

;;folders to be skipped from search
(setq na-exclude-directories (list ".svn" "."))
;;file extensions to be skipped from search.
(setq na-exclude-files (list ".class" ".ico" ".png" ".jar" ".exe" "TAGS" ".DS_Store" ))

(defun na-search-down( directory )
(interactive "DDirectory: ")

(setq regex (read-string "Regex: "))
(setq foundFiles (list )  )
(na-traverse-directory directory  )

(switch-to-buffer "*Result*")
(with-current-buffer "*Result*"
(erase-buffer))
(set (make-local-variable 'truncate-lines) t)

(dolist (file foundFiles )
  (na-search-file-for-regex file regex )))

(defun na-traverse-directory( directory  )
;(print directory)
(dolist (node (directory-files directory t ) )
  (if (file-directory-p node)      
      (progn 
	;;check against excluded directories
	(setq exclude-directory nil)
	(dolist (item na-exclude-directories)
	  (if (string= item (substring node (- 0 (string-width item)) ))
	      (setq exclude-directory t)))
	;;traverse directory
	(if (equal exclude-directory nil )
	    (na-traverse-directory node  ))	
	)
    ;;add found file
    (progn 
      ;;(print node)
      ;;check against excluded files
      (setq exclude-file nil)
      (dolist (item na-exclude-files)
	(if (string= item (substring node (- 0 (string-width item)) ))
	    (setq exclude-file t)))
      (if (equal exclude-file nil )
	  (setq foundFiles (cons node foundFiles)))))))

;; (defun na-test()
;; (interactive)
;; (setq foundFiles (list )  )
;; (na-traverse-directory "~/Projects/")
;; (print foundFiles))


(defun na-search-file-for-regex( file regex )
(setq na-fileName (car (reverse (split-string file "/"))))
(setq na-fileName-printed nil)
(with-temp-buffer
	  (beginning-of-buffer)
	  (insert-file-contents file )
	  (beginning-of-buffer)
	  (while (re-search-forward regex nil t)
	    (setq end (match-end 0))

	    (goto-char end)
	    (setq line (thing-at-point 'line))
	    (setq lineNumber (what-line))

	    (with-current-buffer "*Result*"
	      (end-of-buffer)
	      
	      (if (equal na-fileName-printed nil)
		  (progn 
		    (insert (concat "\n" na-fileName "\n") )
		    (setq na-fileName-printed t)))	      
	      (insert (concat "\t"  lineNumber "\t" line ))))))
