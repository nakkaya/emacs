#!/usr/local/bin/emacs --script

(require 'org)
(require 'ob-tangle)


;; Tangle all files given.
(dolist (file command-line-args-left)
  (princ file)
  (org-babel-tangle-file file))
