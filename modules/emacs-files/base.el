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


;;;
;;; Set up package manager and use-package
;;;

(package-initialize)

(require 'use-package)


;;;
;;; Internal modes that I customize below
;;;
(require 'calendar)
(require 'display-line-numbers)


;;;
;;; General configuration
;;;


;; Put all backup files in a single place
(let ((backup-dir (concat user-emacs-data-directory "/backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))

  (setq backup-directory-alist (cons (cons "." backup-dir) nil)))


;;
;; A bunch of custom variables
;;


(setq
 ;; Disable creation of lock-files named .#<filaname>
 create-lockfiles nil

 ;; Move auto save list files to cache
 auto-save-list-file-prefix (concat user-emacs-cache-directory "/auto-save-list/saves-")

 ;; Paste at point, not at cursor
 mouse-yank-at-point t

 ;; Weeks starts on Mondays
 calendar-week-start-day 1)


;; Forcibly set some variables
(setq-default
 ;; Set tab-size to 4 spaces
 tab-width 4

 ;; Always indent with spaces
 indent-tabs-mode nil)


;; Enable certain modes in all programming/text modes
;;  - Show trailing whitespace
;;  - Highlight current line
;;  - Long line indicator

(let ((my-hook (lambda ()
                 (setq show-trailing-whitespace t)
                 (hl-line-mode 1)
                 (display-line-numbers-mode 1)
                 (display-fill-column-indicator-mode t))))

  (add-hook 'prog-mode-hook my-hook)
  (add-hook 'text-mode-hook my-hook))

(add-hook 'text-mode-hook 'flyspell-mode)

;; Automagic indent on newline
(global-set-key "\C-m" 'newline-and-indent)


;; Bind some keys to manage sizes of buffers easier than with defaults
(progn
  (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "S-C-<down>") 'shrink-window)
  (global-set-key (kbd "S-C-<up>") 'enlarge-window))


;; Unbind suspend frame -- Usually not what you want with graphical emacses
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


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

            ;; Disable the cursor blink
            (blink-cursor-mode 0)

            ;; Enable column number together with line numbers
            (column-number-mode t)))


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
;;; Theme related settings
;;;

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))


(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode t))


(add-to-list 'default-frame-alist '(font . "Liberation Mono 10"))


;;;
;;; Major modes
;;;


;; Go mode
(use-package go-mode
  :ensure t
  :defer 2
  :init (add-hook 'before-save-hook 'gofmt-before-save))


;; PHP mode
(use-package php-mode
  :ensure t
  :defer 2
  :init (setq php-mode-coding-style 'psr2)
  :config (add-hook 'php-mode-hook (lambda ()
                                     (set (make-local-variable 'fill-column) 120))))


;; SCSS mode
(use-package scss-mode
  :ensure t
  :defer 2
  :init (setq scss-compile-at-save nil))


;; Web mode
(use-package web-mode
  :ensure t
  :defer 2
  :mode "\\.twig$"
  :mode "\\.html$"
  :init (setq web-mode-markup-indent-offset 4    ; HTML
              web-mode-css-indent-offset 4       ; CSS
              web-mode-code-indent-offset 4))    ; JS/PHP/etc


;; Rest client mode
(use-package restclient
  :ensure t
  :defer 2
  :mode "\\.rest$"
  ;; Add hook to override C-c C-c in this mode to stay in window
  :init (add-hook 'restclient-mode-hook
                  '(lambda ()
                     (local-set-key
                      (kbd "C-c C-c")
                      'restclient-http-send-current-stay-in-window))))


;; Nix mode
(use-package nix-mode
  :ensure t
  :defer 2
  :mode "\\.nix$"
  :init (setq nix-indent-function 'nix-indent-line))


;;;
;;; Company mode and backends
;;;


(use-package company
  :ensure t
  :defer 2
  ;; TODO: Keybind company-complete to something good
  :bind ("<backtab>" . company-complete)
  ;; Make the tooltip behave well
  :init (setq company-tooltip-minimum-width 15
              company-idle-delay 0.1)
  :config
  (progn
    (use-package company-flx
      :ensure t)
    (use-package company-statistics
      :ensure t
      :init (setq company-statistics-file (concat user-emacs-data-directory "/company-statistics.dat")))


    ;; Completions for restclient mode
    (use-package company-restclient
      :ensure t
      :config
      (add-hook 'restclient-mode-hook
                (lambda ()
                  (set (make-local-variable 'company-backends)
                       '(company-restclient))

                  (company-mode t))))


    ;; Completions for nix options
    (use-package company-nixos-options
      :ensure t
      :config (add-hook 'nix-mode-hook
                        (lambda ()
                          (set (make-local-variable 'company-backends) '(company-nixos-options)))))


    ;; Display details of entries automatically
    (use-package company-quickhelp
      :ensure t
      :config
      (eval-after-load 'company (company-quickhelp-mode)))


    (add-hook 'prog-mode-hook (lambda ()
                                (company-mode t)
                                (company-statistics-mode t)
                                (company-flx-mode t)))))


;;;
;;; LSP
;;;
(use-package eglot
  :ensure t
  :defer 2
  :config (add-to-list 'eglot-server-programs '(php-mode . ("php-language-server")))
  :commands (eglot eglot-ensure)
  :hook ((go-mode . eglot-ensure)
         (php-mode . eglot-ensure)))


;;;
;;; Utilities
;;;


;; Magit
(use-package magit
  :ensure t
  :defer 2
  :bind ("C-x g" . magit-status)     ; Display the main magit popup
  :init
  (setq magit-log-arguments
        '("--graph" "--color" "--decorate" "--show-signature" "-n256")))


;; Interactive search and replace
(use-package anzu
  :ensure t
  :defer 2
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))


;; Flycheck
(use-package flycheck
  :ensure t
  :defer 1
  :init
  (setq flycheck-phpcs-standard "PSR2"
        flycheck-php-phpcs-executable "@phpcs@/bin/phpcs")
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))


;; Webpaste
(use-package webpaste
  :ensure t
  :defer 2
  :bind (("C-c C-p C-p" . webpaste-paste-buffer-or-region)))


;; Which-key to press next
(use-package which-key
  :ensure t
  :defer 2
  :config
  (which-key-mode 1))


;; Inline diff highlight
(use-package diff-hl
  :ensure t
  :defer 2
  :config
  (progn
    (add-hook 'prog-mode-hook 'diff-hl-mode)
    (add-hook 'text-mode-hook 'diff-hl-mode)))


;; Remote debugger for PHP
(use-package geben
  :ensure t
  :defer 2
  :init (setq geben-temporary-file-directory (concat user-emacs-cache-directory "/geben")))


;; Direnv
(use-package direnv
  :ensure t
  :defer 1
  :config (direnv-mode))


;; pdf-tools
(use-package pdf-tools
  :ensure t
  :defer 2
  :config
  (pdf-tools-install))


;; Highlight symbols
(use-package highlight-symbol
  :ensure t
  :defer 2
  :hook (prog-mode . highlight-symbol-mode)
  :init (setq highlight-symbol-idle-delay 0.5
              highlight-symbol-highlight-single-occurrence nil))


;; Highlight indentation for certain modes
(use-package highlight-indent-guides
  :ensure t
  :defer 2
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top)
  :config
  (add-hook 'yaml-mode-hook (lambda ()
                              (highlight-indent-guides-mode t))))


;; Vterm
(use-package vterm
  :ensure t
  :defer 2
  :init (setq vterm-kill-buffer-on-exit t))


;; EditorConfig
(use-package editorconfig
  :ensure t
  :defer 2
  :config (editorconfig-mode 1))


;;;
;;; Helm
;;;

(use-package helm
  :ensure t
  :defer 2
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; Rebind TAB to expand
         ("C-i" . helm-execute-persistent-action)   ; Make TAB work in CLI
         ("C-z" . helm-select-action))              ; List actions using C-z
  :init (setq helm-split-window-inside-p t)
  :config (helm-mode 1))


;; Set up projectile for helm
(use-package helm-projectile
  :ensure t
  :defer 2
  :bind (("C-x , p" . helm-projectile-switch-project)
         ("C-x , f" . helm-projectile-find-file)
         ("C-x , b" . projectile-ibuffer)
         ("C-x , i" . projectile-invalidate-cache)
         ("C-x , a" . helm-projectile-rg)
         ("C-x , k" . projectile-kill-buffers))
  :init (setq projectile-enable-caching t)
  :config (projectile-mode))


;; Helm fuzzier mode
;;(use-package helm-fuzzier
;;  :defer 2
;;  :init (setq helm-mode-fuzzy-match t
;;              helm-M-x-fuzzy-match t
;;              helm-buffers-fuzzy-match t
;;              helm-recentf-fuzzy-match t)
;;  :config (helm-fuzzier-mode 1))


;;;
;;; Org
;;;

;; Highlight code-blocks in org-files
(use-package org
  :ensure t
  :init
  (progn
    (setq org-src-fontify-natively t)
    (add-hook 'org-mode-hook (lambda ()
                               (set (make-local-variable 'fill-column) 77)))))


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


;; Restclient mode for org-babel
(use-package ob-restclient
  :ensure t
  :defer 2
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))


;; Gnuplot
(use-package gnuplot
  :ensure t
  :defer 2
  :bind (("M-C-g" . org-plot/gnuplot))
  :init (setq gnuplot-program "@gnuplot@/bin/gnuplot"))


;;;
;;; Custom functions
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


;;;
;;; Install additional modes
;;;

(use-package centimacro :defer 2 :ensure t)
(use-package dockerfile-mode :defer 2 :ensure t)
(use-package es-mode :defer 2 :ensure t)
(use-package fish-mode :defer 2 :ensure t)
(use-package helm-rg :defer 2 :ensure t)
(use-package helm-nixos-options :defer 2 :ensure t)
(use-package markdown-mode :defer 2 :ensure t)
(use-package vcl-mode :defer 2 :ensure t)
(use-package yaml-mode :defer 2 :ensure t)


;; Rust
(use-package rust-mode :defer 2 :ensure t)
(use-package flycheck-rust :defer 2 :ensure t)
(use-package racer :defer 2 :ensure t)


;;;
;;; Include work utilities
;;;

(let ((work-lisp-dir (expand-file-name "tvnu/.lisp" (getenv "HOME"))))
  (if (file-directory-p work-lisp-dir)
      (progn
        ;; Add work lisp dir to load path
        (add-to-list 'load-path work-lisp-dir)

        (require 'work-init))))


;;; base.el ends here
