;;; base.el -- Global emacs config starts here
;;; Commentary:
;;; Code:

;; Define XDG directories
(defvar user-emacs-config-directory
  (concat (getenv "HOME") "/.config/emacs"))

(defvar user-emacs-data-directory
  (concat (getenv "HOME") "/.local/share/emacs"))

(defvar user-emacs-cache-directory
  (concat (getenv "HOME") "/.cache/emacs"))

;; Avoid creation of ~/.emacs.d/
(setq-default user-emacs-directory user-emacs-data-directory)

;; Set up package manager
(package-initialize)

(require 'use-package)


;;;
;;;
;;; General configuration
;;;
;;;


;; Add a startup hook that logs the startup time to the messages buffer
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Put all backup files in a single place
(let ((backup-dir (concat user-emacs-data-directory "/backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))

  (setq backup-directory-alist (cons (cons "." backup-dir) nil)))

;; Disable creation of lock-files named .#<filaname>
(setq create-lockfiles nil)

;; Move auto save list files to cache
(setq auto-save-list-file-prefix
      (concat user-emacs-cache-directory "/auto-save-list/saves-"))

;; Hide startup screen
(setq inhibit-startup-screen t)

;; A bunch of custom vars
;; TODO: Do all need setq-default?
(setq-default mouse-yank-at-point t      ; Paste at cursor, not at mouse
              show-trailing-whitespace t ; Highlight trailing whitespaces
              calendar-week-start-day 1  ; Weeks starts on Mondays
              tab-width 4                ; Set tab-size to 4 spaces
              indent-tabs-mode nil)      ; Always indent with spaces

;; Automagic indent on newline
(global-set-key "\C-m" 'newline-and-indent)

;; Make sure that there is one newline at the end of the file while saving,
;; also removes all spaces at the end of lines.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Change all yes-or-no-p to y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; Slow things to load after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Auto reread from disk when file changes
            (global-auto-revert-mode t)

            ;; Enable line highlight mode everywhere
            (global-hl-line-mode 1)

            ;; Enable Winner Mode
            (winner-mode 1)

            ;; Disable line wrapping where the window ends
            (toggle-truncate-lines t)

            ;; Hide menubar, toolbar and scrollbar
            (menu-bar-mode -1)
            (tool-bar-mode -1)
            (if (boundp 'scroll-bar-mode)
                (scroll-bar-mode -1))

            ;; Highlight parenthesises
            (show-paren-mode t)

            ;; Enable column number together with line numbers
            (column-number-mode t)))

;; Setup theme
(use-package dracula-theme
  :config
  (progn
	(load-theme 'dracula t)

    ;; Required to be able to override org-level-faces
    (require 'org)

	(mapc (lambda (args)
			(face-spec-reset-face (car args))
			(apply 'set-face-attribute args))
		  '((font-lock-variable-name-face nil :foreground "#ffb86c")
			(org-level-1 nil :inherit bold :foreground "#ff79c6")
			(org-level-2 nil :inherit bold :foreground "#bd93f9")))))

(use-package telephone-line
  :config
  (progn
	(telephone-line-mode 1)))

;; Set font -- https://stackoverflow.com/questions/3984730/emacs-gui-with-emacs-daemon-not-loading-fonts-correctly
(add-to-list 'default-frame-alist '(font . "Liberation Mono 10"))

;; Set cursor color -- https://emacs.stackexchange.com/questions/13291/emacs-cursor-color-is-different-in-daemon-and-non-daemon-modes
(progn
  (require 'frame)

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (modify-frame-parameters
               frame (list (cons 'cursor-color "White"))))))

;; Enable line number mode in programming modes
(progn
  (require 'linum)

  (setq linum-format "%3d ")
  (add-hook 'prog-mode-hook 'linum-mode))

;; Unbind suspend frame -- Usually not what you want with graphical emacses
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Remap beginning-of-line to ignore indentation
(defun my/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
   point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Remap C-a to `my/move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'my/move-beginning-of-line)


;;;
;;;
;;; Major modes
;;;
;;;


;; Go mode
(use-package go-mode
  :defer 2
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

;; PHP mode
(use-package php-mode
  :defer 2
  :config
  (setq php-mode-coding-style 'psr2))

;; SCSS mode
(use-package scss-mode
  :defer 2
  :config
  (setq scss-compile-at-save nil))

;; Web mode
(use-package web-mode
  :defer 2
  :mode "\\.twig$"
  :mode "\\.html$"
  :config
  (setq web-mode-markup-indent-offset 4    ; HTML
        web-mode-css-indent-offset 4       ; CSS
        web-mode-code-indent-offset 4))    ; JS/PHP/etc

;; Rest client mode
(use-package restclient
  :defer 2
  :mode "\\.rest$"
  :config
  ;; Add hook to override C-c C-c in this mode to stay in window
  (add-hook 'restclient-mode-hook
            '(lambda ()
               (local-set-key
                (kbd "C-c C-c")
                'restclient-http-send-current-stay-in-window))))

;; Nix mode
(use-package nix-mode
  :defer 2
  :mode "\\.nix$"
  :config (setq nix-indent-function 'nix-indent-line))


;;;
;;;
;;; Company mode and backends
;;;
;;;


;; Company mode
(use-package company
  :defer 2
  :diminish company-mode
  :bind ("<backtab>" . company-complete)
  :config
  (progn
    ;; TODO: Keybind company-complete to something good

    ;; Make the tooltip behave well
    (setq company-tooltip-minimum-width 15
          company-idle-delay 0.1)

    (global-company-mode)

    ;; Fuzzzy matching
    (use-package company-flx
      :config
      (company-flx-mode +1))

    ;; Statistics for completions
    (use-package company-statistics
      :config
      (progn
        (setq company-statistics-file
              (concat user-emacs-data-directory "/company-statistics.dat"))
        (company-statistics-mode)))

    ;; Completions for restclient mode
    (use-package company-restclient
      :config
      (add-hook 'restclient-mode-hook
                (lambda ()
                  (set (make-local-variable 'company-backends)
                       '(company-restclient))

                  (company-mode t))))

    ;; ctags and stuff for php completions
    (use-package company-php
      :config
      (progn
        (use-package php-mode)

        (setq ac-php-tags-path (concat user-emacs-cache-directory "/ac-php"))

        (add-hook 'php-mode-hook
                  '(lambda ()
                     (unless (executable-find "ctags")
                       (error "Program: ctags is missing"))

                     ;; Add build company-backends with dabbrev and ac-php
                     (set (make-local-variable 'company-backends)
                          '((company-dabbrev-code
                             company-gtags
                             company-etags
                             company-keywords
                             company-ac-php-backend)))

                     (company-mode t)))

        (define-key php-mode-map (kbd "C-]") 'ac-php-find-symbol-at-point)
        (define-key php-mode-map (kbd "C-t") 'ac-php-location-stack-back)))

    ;; Completions for go code
    (use-package company-go
      :config
      (progn
        (add-hook 'go-mode-hook
                  (lambda ()
                    (unless (executable-find "gocode")
                      (error "Program: gocode is missing"))

                    (set (make-local-variable 'company-backends) '(company-go))
                    (company-mode t)))))))


;;;
;;;
;;; Utilities
;;;
;;;


;; Interactive search and replace
(use-package anzu
  :defer 2
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; Keybind magit
(use-package magit
  :defer 2
  :bind (("C-x g" . magit-status)     ; Display the main magit popup
         ("C-x M-g" . magit-dispatch-popup)) ; Display keybinds for magit
  :config
  (setq magit-log-arguments
        '("--graph" "--color" "--decorate" "--show-signature" "-n256")))

;; Flycheck
(use-package flycheck
  :defer 2
  :init
  (setq flycheck-phpcs-standard "PSR2"
        flycheck-php-phpcs-executable "phpcs")
  :config
  (global-flycheck-mode))

;; Eyebrowse
(use-package eyebrowse
  :defer 2
  :config
  (progn
    (setq eyebrowse-new-workspace t)
    (eyebrowse-mode)))

;; Webpaste
(use-package webpaste
  :defer 2
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)))

;; Yasnippet (Yet Another Snippet Extension)
(use-package yasnippet
  :defer 2
  :config
  (progn
    (let ((yas-dir (concat user-emacs-data-directory "/snippets")))
      (unless (file-directory-p yas-dir)
        (mkdir yas-dir t))

      (setq yas-snippet-dirs
            (cons yas-dir '(yas-installed-snippets-dir))))

    (yas-global-mode 1)))

;; Which-key to press next
(use-package which-key
  :defer 2
  :config
  (which-key-mode 1))

;; Smooth scrolling mode
(use-package smooth-scrolling
  :defer 2
  :init
  (setq smooth-scroll-margin 2)
  :config
  (smooth-scrolling-mode))

;; Inline diff highlight
(use-package diff-hl
  :defer 2
  :config
  (global-diff-hl-mode))

;; Long lines highlight
(use-package column-enforce-mode
  :defer 2
  :config
  (progn
    (face-spec-set column-enforce-face '((t (:background "dark red"))))

    ;; Make/Add hooks
    (add-hook 'prog-mode-hook 'column-enforce-mode)
    (add-hook 'php-mode-hook (make-column-rule 120))
    (add-hook 'org-mode-hook (make-column-rule 77))))

;; Remote debugger for PHP
(use-package geben
  :defer 2
  :config
  (setq geben-temporary-file-directory
        (concat user-emacs-cache-directory "/geben")))

;; Direnv
(use-package direnv
  :defer 1
  :config
  (direnv-mode))


;;;
;;;
;;; Helm
;;;
;;;


;; Set up Helm
(use-package helm
  :defer 2
  :diminish helm-mode
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; Rebind TAB to expand
         ("C-i" . helm-execute-persistent-action) ; Make TAB work in CLI
         ("C-z" . helm-select-action)) ; List actions using C-z
  :config
  (progn
    (setq helm-split-window-inside-p t)
    (helm-mode 1)))

;; Set up projectile for helm
(use-package helm-projectile
  :defer 2
  :bind (("C-x , p" . helm-projectile-switch-project)
         ("C-x , f" . helm-projectile-find-file)
         ("C-x , b" . projectile-ibuffer)
         ("C-x , i" . projectile-invalidate-cache)
         ("C-x , a" . helm-projectile-ag)
         ("C-x , k" . projectile-kill-buffers))
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode))

;; Helm fuzzier mode
(use-package helm-fuzzier
  :defer 2
  :init
  ;; TODO: Move variables to correct places with correct requires
  (setq helm-mode-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-match t
        helm-recentf-fuzzy-match t)
  :config
  (helm-fuzzier-mode 1))


;;;
;;;
;;; Org
;;;
;;;


;; Highlight code-blocks in org-files
(use-package org
  :init
  (setq org-src-fontify-natively t))

;; Workhour clocking settings
(use-package org
  :defer 2
  :init
  ;; This changes formating of time in clocktables
  ;; So instead of "Dd HH:MM" we get "HH.MM"
  ;; TODO: Change from this variable since it's deprecated and do the correct require
  (setq org-time-clocksum-use-fractional t)
  :config
  ;; Add advice to override indention
  ;; Define function to re-do indent of items in clocktable
  ;; http://emacs.stackexchange.com/a/9544
  (advice-add
   'org-clocktable-indent-string
   :override (lambda (level)
               (if (= level 1)
                   ""
                 (let ((str " "))
                   (while (> level 2)
                     (setq level (1- level)
                           str (concat str "-")))
                   (concat str "- "))))))

;; Gnuplot
(use-package gnuplot
  :defer 2
  :bind (("M-C-g" . org-plot/gnuplot)))


;;;
;;;
;;; Custom functions
;;;
;;;


;; Three column layout
(defun three-column-layout ()
  "Split into three equaly sized columns."
  (interactive)
  (let ((new-size (/ (window-total-width) 3)))
    (split-window-right new-size)
    (other-window 1)
    (split-window-right new-size)
    (other-window -1)

    (message "Splitted to three-column-layout")))

;; Toggle color scheme
;; TODO: Needs to be updated for the theme I actually use
(if (string= window-system "x")
    (progn
      (defun toggle-color-theme ()
        (interactive)
        (if (equal (car custom-enabled-themes) 'tango-dark)
            (load-theme 'adwaita)
          (load-theme 'tango-dark)))

      (global-set-key [f12] 'toggle-color-theme)))


;;;
;;;
;;; Install additional modes
;;;
;;;


(use-package centimacro :defer 2)
(use-package es-mode :defer 2)
(use-package fish-mode :defer 2)
(use-package helm-ag :defer 2)
(use-package markdown-mode :defer 2)
(use-package phpcbf :defer 2)
(use-package vcl-mode :defer 2)
(use-package yaml-mode :defer 2)


;;;
;;;
;;; Include work utilities
;;;
;;;


(let ((work-lisp-dir (expand-file-name "tvnu/.lisp" (getenv "HOME"))))
  (if (file-directory-p work-lisp-dir)
      (progn
        ;; Add work lisp dir to load path
        (add-to-list 'load-path work-lisp-dir)

        (require 'work-init))))


;;; base.el ends here
