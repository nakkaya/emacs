(setq package-list
      '(use-package
         exwm))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(use-package exwm :ensure t
  :config 
  (use-package exwm-config
    :config (exwm-config-default)))

;;-f exwm-enable
;;--eval \"(exwm-init)\""
