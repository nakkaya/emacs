#!/usr/local/bin/emacs --script

(if (not (string= (car argv) "--"))
    (setq argv (cons "--" argv)))

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(setq package-list
      '(use-package
         org
         commander))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'commander)
(require 'org)
(require 'ob-tangle)

(setq tramp-verbose 0
      org-id-locations-file nil)

(setq tangle-host nil)
(setq tangle-files '())

(defun tangle (&rest files)
  "List of Files to Tangle."
  (setq tangle-files files))

(defun host (host)
  "Host to Tangle to."
  (setq tangle-host host))

(defun tangle-to-host (host)
  (while (re-search-forward ":tangle\s-*?\\(.*?\\)\s-*?" nil t)
    (replace-match
     (concat "/ssh:" host ":" (match-string 1)) nil nil nil 1)))

(commander
 (name "ob-tangle")
 (option "--host <host>" host nil)
 (option "--tangle <*>" tangle)
 (option "--help" "Show usage information" commander-print-usage))

(dolist (file tangle-files)
  (let* ((data (with-temp-buffer
                 (insert-file-contents file)
                 (beginning-of-buffer)
                 (if tangle-host
                     (tangle-to-host tangle-host))
                 (buffer-string)))
         (t_file (make-temp-file file nil ".org" data)))
    (org-babel-tangle-file t_file)))
