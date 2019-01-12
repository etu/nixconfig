;;; eshell.el -- My eshell customizations
;;; Commentary:
;;; Code:

(require 'cl)
(require 'dash)
(require 'em-prompt)
(require 'magit)
(require 's)



(defvar my/eshell-prompt-sections '()
  "My registred eshell prompt sections.")

(defvar my/eshell-separator " | "
  "Separator between esh-sections.")

(defvar my/eshell-section-delimiter " "
  "Separator between an esh-section icon and form.")

(defvar my/eshell-header "┌─ "
  "Eshell prompt header.")

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(defvar my/eshell-prompt-string "└─> ")
(defvar my/eshell-prompt-regexp "└─> ")



(defun my/eshell-with-face (str &rest props)
  "Return STR propertized with PROPS."
  (propertize str 'face (list props)))

(defun my/eshell-prompt-section (icon form &rest props)
  "Build eshell section with ICON prepended to evaluated FORM with PROPS."
  (when (eval form)
    (-> icon
        (concat my/eshell-section-delimiter (eval form))
        (my/eshell-with-face props))))



;;  (list-unordered icon from emacs-all-the-icons)
(progn
  (defvar my/eshell-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq my/eshell-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq my/eshell-prompt-num (incf my/eshell-prompt-num))))

  (add-to-list 'my/eshell-prompt-sections '("\xf061"
                                            (number-to-string my/eshell-prompt-num)
                                            '(:foreground "brown"))))

;;  (clock icon from emacs-all-the-icons)
(add-to-list 'my/eshell-prompt-sections '("\xf046"
                                          (format-time-string "%H:%M" (current-time))
                                          '(:foreground "forest green")))

;;  (git icon)
(add-to-list 'my/eshell-prompt-sections '("\xe907"
                                          (magit-get-current-branch)
                                          '(:foreground "pink")))

;;  (file-directory icon from emacs-all-the-icons)
(add-to-list 'my/eshell-prompt-sections '("\xf016"
                                          (abbreviate-file-name (eshell/pwd))
                                          '(:foreground "gold" :bold ultra-bold)))



(defun my/eshell-prompt-function ()
  "Build my own `eshell-prompt-function'."
  ;; base prompt on the header
  (let ((prompt my/eshell-header))
    ;; Loop sections
    (dolist (section my/eshell-prompt-sections)
      (let ((section-output (apply 'my/eshell-prompt-section section)))
        ;; Add the section
        (when section-output
          ;; If we're not on the first element, add the separator
          (unless (eq section (nth 0 my/eshell-prompt-sections))
            (setq prompt (concat prompt my/eshell-separator)))

          (setq prompt (concat prompt section-output)))))

    ;; Add the prompt string
    (concat prompt "\n" my/eshell-prompt-string)))

(setq eshell-prompt-function 'my/eshell-prompt-function)



(defun my/eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq my/eshell-prompt-num 0)      ; Reset the counter on clear
    (eshell-emit-prompt)))

;; Bind the clear keybind for eshell buffers
;; Also disable the display of trailing whitespace
(add-hook 'eshell-mode-hook
          '(lambda ()
             (setq-local show-trailing-whitespace nil)
             (local-set-key (kbd "C-l") 'my/eshell-clear-buffer)))

;;; eshell.el ends here
