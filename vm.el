(load-file "init.el")

(setq package-list
      '(use-package
         exwm))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)

(exwm-randr-enable)
(exwm-config-default)
