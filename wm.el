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

;;; Rename buffer to window title.
(defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

(push ?\C-x exwm-input-prefix-keys)
(exwm-input-set-key (kbd "C-x ]") #'ibuffer)
(exwm-input-set-key (kbd "C-x C-]") #'ibuffer)
(exwm-input-set-key (kbd "C-x \\") #'other-window)
(exwm-input-set-key (kbd "C-x C-\\") #'other-window)

(exwm-input-set-key
 (kbd "C-x f")
 (lambda () (interactive)
   (start-process "browser" "*Messages*" "firefox")))

