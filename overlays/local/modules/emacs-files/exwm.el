;;; exwm.el -- My EXWM init file
;;; Commentary:
;;; Code:


(require 'exwm)
(require 'exwm-config)

;; Display time in modeline
(progn
  (require 'time)
  (setq display-time-24hr-format t)
  (display-time-mode 1))

;; Display battery mode
(progn
  (require 'battery)
  (display-battery-mode))

;; Define a function to easily run commands
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
  (exwm-input-set-key (kbd "s-t") 'exwm-run-terminal))

;; Define desktop environment commands
(progn
  (require 'desktop-environment)
  (setq desktop-environment-screenlock-command "@i3lockCommand@")
  (setq desktop-environment-screenshot-directory "~"
        desktop-environment-screenshot-command "@flameshot@/bin/flameshot gui"
        desktop-environment-screenshot-partial-command "@flameshot@/bin/flameshot gui")
  (setq desktop-environment-brightness-get-command "@xbacklight@/bin/xbacklight"
        desktop-environment-brightness-set-command "@xbacklight@/bin/xbacklight %s"
        desktop-environment-brightness-get-regexp "\\([0-9]+\\)"
        desktop-environment-brightness-normal-increment "-inc 10"
        desktop-environment-brightness-normal-decrement "-dec 10"
        desktop-environment-brightness-small-increment "-inc 5"
        desktop-environment-brightness-small-decrement "-dec 5")
  (desktop-environment-mode))

;; Set up systray
(progn
  (require 'exwm-systemtray)
  (exwm-systemtray-enable))

;; Set up randr support
(progn
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(1 "eDP1" 9 "HDMI1"))
  (exwm-randr-enable))

(progn
  ;; Bind switch to workspace commands
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))

  ;; Make it possible to do exwm-reset
  (exwm-input-set-key (kbd "s-r") 'exwm-input-grab-keyboard)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (let ((tilde-exwm-title
                     (replace-regexp-in-string (getenv "HOME") "~" exwm-title)))
                (exwm-workspace-rename-buffer (format "%s: %s" exwm-class-name tilde-exwm-title)))))

  (exwm-enable)
  (exwm-init))

;;; exwm.el ends here
