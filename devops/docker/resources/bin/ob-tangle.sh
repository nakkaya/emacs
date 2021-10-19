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

(setq tangle-host '())
(setq tangle-files '())

(defun tangle (&rest files)
  "List of Files to Tangle."
  (setq tangle-files files))

(defun host (&rest  hosts)
  "List of Hosts to Tangle to."
  (setq tangle-hosts hosts))

(defun tangle-to-host (host)
  (while (re-search-forward ":tangle\s-*?\\(.*?\\)\\($\\|\s-*?\\)" nil t)
    (replace-match
     (concat "/ssh:" host ":" (match-string 1)) nil nil nil 1)))

(commander
 (name "ob-tangle")
 (option "--host <*>" host)
 (option "--tangle <*>" tangle)
 (option "--help" "Show usage information" commander-print-usage))

(dolist (host tangle-hosts)
  (dolist (file tangle-files)
    (let* ((data (with-temp-buffer
                   (insert-file-contents file)
                   (beginning-of-buffer)
                   (tangle-to-host host)
                   (buffer-string)))
           (t_file (make-temp-file file nil ".org" data)))
      (org-babel-tangle-file t_file))))
