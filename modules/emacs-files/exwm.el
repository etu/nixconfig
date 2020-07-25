;;; exwm.el -- My EXWM init file
;;; Commentary:
;;; Code:

(require 'use-package)

;; Define desktop environment commands
(use-package desktop-environment
  :ensure t
  :init (setq
         ;; Lock command
         desktop-environment-screenlock-command "@lockCommand@"

         ;; Screenshot settings
         desktop-environment-screenshot-directory "~"
         desktop-environment-screenshot-command "@flameshot@/bin/flameshot gui"
         desktop-environment-screenshot-partial-command "@flameshot@/bin/flameshot gui"

         ;; Screen brightness settings
         desktop-environment-brightness-get-command "@xbacklight@/bin/xbacklight"
         desktop-environment-brightness-set-command "@xbacklight@/bin/xbacklight %s"
         desktop-environment-brightness-get-regexp "\\([0-9]+\\)"
         desktop-environment-brightness-normal-increment "-inc 10"
         desktop-environment-brightness-normal-decrement "-dec 10"
         desktop-environment-brightness-small-increment "-inc 5"
         desktop-environment-brightness-small-decrement "-dec 5")
  :config (desktop-environment-mode 1))

;; Display time in modeline
(use-package time
  :init (setq display-time-24hr-format t)
  :config (add-hook 'exwm-init-hook 'display-time-mode))

;; Display battery mode
(use-package battery
  :config (add-hook 'exwm-init-hook 'display-battery-mode))

;; Define a function to easily run commands
(use-package exwm
  :ensure t
  :config
  (progn
    (defun exwm-run-systemd (command)
      (interactive (list (read-shell-command "$ ")))
      (let ((cmd (concat "@systemd@/bin/systemd-run --user " command)))
        (start-process-shell-command cmd nil cmd)))
    (exwm-input-set-key (kbd "s-SPC") 'exwm-run-systemd)
    (exwm-input-set-key (kbd "s-e") 'exwm-run-systemd)

    (defun exwm-run (command)
      "Run COMMAND."
      (interactive (list (read-shell-command "> ")))
      (start-process-shell-command command nil command))
    (exwm-input-set-key (kbd "C-s-SPC") 'exwm-run)

    ;; Special function to run the terminal
    (defun exwm-run-terminal ()
      (interactive)
      (exwm-run-systemd "@kitty@/bin/kitty"))
    (exwm-input-set-key (kbd "s-t") 'exwm-run-terminal)

    (defun exwm-run-rofi-emoji ()
      (interactive)
      (exwm-run "@rofi@/bin/rofi -show emoji -theme glue_pro_blue"))
    (exwm-input-set-key (kbd "s-u") 'exwm-run-rofi-emoji)

    ;; Bind switch to workspace commands
    (dotimes (i 10)
      (exwm-input-set-key (kbd (format "s-%d" i))
                          `(lambda ()
                             (interactive)
                             (exwm-workspace-switch-create ,i))))

    ;; Make it possible to do exwm-reset
    (exwm-input-set-key (kbd "s-r") 'exwm-input-grab-keyboard)

    ;; Some hooks to tweak exwm
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))

    (add-hook 'exwm-update-title-hook
              (lambda ()
                (let ((tilde-exwm-title
                       (replace-regexp-in-string (getenv "HOME") "~" exwm-title)))
                  (exwm-workspace-rename-buffer (format "%s: %s" exwm-class-name tilde-exwm-title)))))

    ;; Enable randr
    (use-package exwm-randr :config (exwm-randr-enable))

    ;; Enable systray
    (use-package exwm-systemtray :config (exwm-systemtray-enable))

    ;; Start exwm
    (exwm-enable)
    (exwm-init)

    ;; Loop through and create all workspaces
    (dolist (i '(0 9 8 7 6 5 4 3 2 1))
      (exwm-workspace-switch-create i))))

;;; exwm.el ends here
