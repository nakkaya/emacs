#!/usr/local/bin/emacs --script

(setq tramp-verbose 0
      org-id-locations-file nil)

(require 'org)
(require 'ob-tangle)

(dolist (file command-line-args-left)
  (org-babel-tangle-file file))
