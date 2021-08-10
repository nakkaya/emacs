(setq package-list
      '(use-package
           elfeed
           jupyter
           pdf-tools
           org-noter))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Emacs Jupyter
;;

(require 'jupyter)
(require 'ob-jupyter)

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((jupyter . t))))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:session . "py")
                                                     (:results . "raw drawer")))

;; PDF Tools
;;

(use-package pdf-tools
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package org-noter
  :after (:any org pdf-tools)
  :config
  (setq
   org-noter-always-create-frame nil
   org-noter-hide-other t
   org-noter-notes-search-path na-agenda-folder
   org-noter-auto-save-last-location t)
  :ensure t)

;; elfeed

(require 'elfeed)

(setq elfeed-db-directory "/storage/.elfeed"
      elfeed-sort-order   'ascending)

(run-with-idle-timer (* 2 60 60) t #'elfeed-update)

(define-key elfeed-show-mode-map (kbd "j") 'elfeed-show-next)
(define-key elfeed-show-mode-map (kbd "k") 'elfeed-show-prev)

(setq elfeed-feeds
      '("https://news.ycombinator.com/rss"
        "https://www.reddit.com/r/lisp/.rss"
        "http://lisptips.com/rss"
        "http://planet.lisp.org/rss20.xml"
        "https://www.reddit.com/r/cpp/.rss"
        "https://www.reddit.com/r/MachineLearning/.rss"
        "https://www.reddit.com/r/emacs/.rss"
        "https://www.reddit.com/r/Clojure/.rss"
        "https://www.reddit.com/r/artificial/.rss"
        "http://www.scheme.dk/planet/atom.xml"
        "http://feeds.feedburner.com/clojure"
        "http://lambda-the-ultimate.org/rss.xml"
        "https://www.reddit.com/r/scheme/.rss"
        "http://www.masteringemacs.org/feed/"
        "http://lisp-univ-etc.blogspot.com/feeds/posts/default/-/en"
        "https://www.reddit.com/r/Racket/.xml"
        ;;robotics
        "https://www.reddit.com/r/ControlTheory/.rss"
        "http://feeds.hizook.com/Hizook"
        "https://www.reddit.com/r/robotics/.rss"
        "http://feeds2.feedburner.com/IeeeSpectrum"
        "http://feeds.feedburner.com/ieeespectrum/automaton"
        "http://www.raspberrypi.org/feed"
        "http://feeds2.feedburner.com/hackaday/LgoM"
        "https://www.schneier.com/blog/atom.xml"
        "http://git-annex.branchable.com/design/assistant/blog/index.rss"
        "http://feeds.feedburner.com/ServerPorn"
        "http://blog.jgc.org/feeds/posts/default?alt=rss"
        "http://hannahfry.co.uk/feed/"
        "http://tromey.com/blog/?feed=rss2"
        "http://scanlime.org/feed/"
        "http://hooverphonic-bootlegs.blogspot.com/feeds/posts/default"
        "http://www.hilarymason.com/feed/"
        "http://xkcd.com/atom.xml"
        "http://www.boston.com/bigpicture/index.xml"
        "http://feeds.feedburner.com/theatlantic/infocus"
        "https://www.reddit.com/r/selfhosted/.rss"
        "https://www.reddit.com/r/KerasML/.rss"
        "https://www.reddit.com/r/tensorflow/.rss"
        "https://www.reddit.com/r/PostgreSQL/.rss"
        "https://www.reddit.com/r/aws/.rss"
        "https://www.reddit.com/r/terraform/.rss"))

;; Init
;;

(setq server-socket-dir "/opt/emacsd/server")
(setq server-name "emacsd")
(defun server-ensure-safe-dir (dir) "Noop" t)
(server-start)
(set-face-attribute 'default nil :height 125)
(blink-cursor-mode)
