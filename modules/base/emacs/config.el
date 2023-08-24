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

;; Set some variables.
(setq-default
 ;; Don't show the welcome splash screen.
 inhibit-startup-screen t

 ;; Avoid creation of ~/.emacs.d/
 user-emacs-directory user-emacs-data-directory)


;;;
;;; Set up package manager and use-package
;;;

(package-initialize)

(require 'use-package)
(setq use-package-always-defer t)


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
 calendar-week-start-day 1

 ;; Change all yes-or-no-p to y-or-n-p
 use-short-answers t)


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
  :demand t
  :config (load-theme 'dracula t))


(use-package telephone-line
  :ensure t
  :demand t
  :config (telephone-line-mode t))


(add-to-list 'default-frame-alist '(font . "@fontname@ @fontsize@"))


;; Use Noto Color Emoji for symbols
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append))


;;;
;;; Install additional modes without much configuration
;;;

(use-package centimacro :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package es-mode :ensure t)
(use-package fish-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package vcl-mode :ensure t)
(use-package yaml-mode :ensure t)


;;;
;;; Major modes
;;;


;; Go mode
(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save))


;; PHP mode
(use-package php-mode
  :ensure t
  :init (setq php-mode-coding-style 'psr2)
  :config (add-hook 'php-mode-hook (lambda ()
                                     (set (make-local-variable 'fill-column) 120))))


;; SCSS mode
(use-package scss-mode
  :ensure t
  :init (setq scss-compile-at-save nil))


;; Web mode
(use-package web-mode
  :ensure t
  :mode "\\.twig$"
  :mode "\\.html$"
  :init (setq web-mode-markup-indent-offset 4    ; HTML
              web-mode-css-indent-offset 4       ; CSS
              web-mode-code-indent-offset 4))    ; JS/PHP/etc


;; Rest client mode
(use-package restclient
  :ensure t
  :mode "\\.rest$"
  ;; Add hook to override C-c C-c in this mode to stay in window
  :init (add-hook 'restclient-mode-hook
                  (lambda ()
                    (local-set-key
                     (kbd "C-c C-c")
                     'restclient-http-send-current-stay-in-window))))


;; Nix mode
(use-package nix-mode
  :ensure t
  :mode "\\.nix$"
  :init (setq nix-indent-function 'nix-indent-line))


;; Set up Vue mode
(use-package vue-mode
  :ensure t
  :config
  (progn
    (add-hook 'vue-mode-hook (lambda ()
                               (set (make-local-variable 'tab-width) 2)
                               (setq js-indent-level 2
                                     css-indent-offset 2
                                     vue-html-extra-indent 2)))))


;; SCAD Mode
(use-package scad-mode
  :ensure t)

(use-package scad-preview
  :ensure t
  :init
  (setq image-use-external-converter t
        scad-preview-colorscheme "BeforeDawn"))

;;;
;;; Company mode and backends
;;;


(use-package company
  :ensure t
  ;; TODO: Keybind company-complete to something good
  :bind ("<backtab>" . company-complete)
  ;; Make the tooltip behave well
  :init (setq company-tooltip-minimum-width 15
              company-idle-delay 0.1)
  :hook (prog-mode . company-mode))


;; Fuzzy matching in company
;; (w-t-b => (with-temp-buffer
(use-package company-flx
  :ensure t
  :hook (company-mode . company-flx-mode))


;; Statustics based ranking in company
(use-package company-statistics
  :ensure t
  :init (setq company-statistics-file (concat user-emacs-data-directory "/company-statistics.dat"))
  :hook (company-mode . company-statistics-mode))


;; Display details of entries automatically
(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))


;; Completions for restclient mode
(use-package company-restclient
  :ensure t
  :config (add-hook 'restclient-mode-hook
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


;;;
;;; LSP
;;;
(use-package eglot
  :ensure t
  :commands eglot-ensure
  :hook ((css-mode dockerfile-mode go-mode js-mode nix-mode php-mode scss-mode sh-mode) . eglot-ensure)
  :config (progn
            (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio")))
            (add-to-list 'eglot-server-programs '((php-mode phps-mode) . ("intelephense" "--stdio")))
            (add-to-list 'eglot-server-programs '((scss-mode css-mode) . ("css-languageserver" "--stdio")))))


;;;
;;; Utilities
;;;


;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)     ; Display the main magit popup
  :init (setq magit-log-arguments
              '("--graph" "--color" "--decorate" "--show-signature" "-n256")))


;; Interactive search and replace
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))


;; Flycheck
(use-package flycheck
  :ensure t
  :hook flycheck-mode
  :init (setq flycheck-phpcs-standard "PSR12"
              flycheck-php-phpcs-executable "phpcs"))


;; Webpaste
(use-package webpaste
  :ensure t
  :bind (("C-c C-p C-p" . webpaste-paste-buffer-or-region)))


;; Which-key to press next
(use-package which-key
  :ensure t
  :config (which-key-mode 1))


;; Inline diff highlight
(use-package diff-hl
  :ensure t
  :hook ((prog-mode text-mode) . diff-hl-mode))

;; Remote debugger for PHP
(use-package geben
  :ensure t
  :init (setq geben-temporary-file-directory (concat user-emacs-cache-directory "/geben")))


;; Direnv
(use-package direnv
  :ensure t
  :config (direnv-mode))


;; pdf-tools
(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))


;; Highlight symbols
(use-package highlight-symbol
  :ensure t
  :hook (prog-mode . highlight-symbol-mode)
  :init (setq highlight-symbol-idle-delay 0.5
              highlight-symbol-highlight-single-occurrence nil))


;; Highlight indentation for certain modes
(use-package highlight-indent-guides
  :ensure t
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top)
  :hook (yaml-mode . highlight-indent-guides-mode))


;; Vterm
(use-package vterm
  :ensure t
  :init (setq vterm-kill-buffer-on-exit t))


;; EditorConfig
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))


;;;
;;; Helm
;;;

(use-package helm
  :ensure t
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

(use-package helm-rg :ensure t)

;; Set up projectile for helm
(use-package helm-projectile
  :ensure t
  :bind (("C-x , p" . helm-projectile-switch-project)
         ("C-x , f" . helm-projectile-find-file)
         ("C-x , b" . projectile-ibuffer)
         ("C-x , i" . projectile-invalidate-cache)
         ("C-x , a" . helm-projectile-rg)
         ("C-x , k" . projectile-kill-buffers))
  :init (setq projectile-enable-caching t)
  :config (projectile-mode))


;;;
;;; Org
;;;

;; Highlight code-blocks in org-files
(use-package org
  :ensure t
  :init
  (progn
    (setq org-src-fontify-natively t)

    ;; Set path for org agenda files
    (if (file-directory-p "@dataPrefix@/home/etu/.dotfiles/skeleton/org")
        (setq org-agenda-files '("@dataPrefix@/home/etu/.dotfiles/skeleton/org")))

    ;; Disable indentation of text and content to be based on the headlines
    ;; amount of stars.
    (setq org-adapt-indentation nil)

    (add-hook 'org-mode-hook (lambda ()
                               (set (make-local-variable 'fill-column) 77)))))


;; Workhour clocking settings
(use-package org
  ;; This changes formating of time in clocktables from "1d HH:MM" to "HH:MM"
  :init (setq org-duration-format '((special . h:mm)))

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
  :config (org-babel-do-load-languages
           'org-babel-load-languages
           '((restclient . t))))


;; Gnuplot
(use-package gnuplot
  :ensure t
  :bind (("M-C-g" . org-plot/gnuplot)))


;; GPTel
(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode 'org-mode)
  (setq-default gptel-api-key
        (lambda ()
          (string-trim-right (shell-command-to-string "pass API/openai.com")))))


;; Copilot.el
(use-package copilot
  :commands (copilot-mode)
  :hook ((dockerfile-mode php-mode nix-mode go-mode lisp-mode yaml-mode markdown-mode) . copilot-mode)
  :bind (("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-<return>" . 'copilot-accept-completion-by-line)))


;;;
;;; Load extra config provided from other parts of the nixos
;;; configuration rather than hard coded in the main config file.
;;;
@extraConfig@



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
;;; Include work utilities
;;;

(let ((work-lisp-dir (expand-file-name "tvnu/.lisp" (getenv "HOME"))))
  (if (file-directory-p work-lisp-dir)
      (progn
        ;; Add work lisp dir to load path
        (add-to-list 'load-path work-lisp-dir)

        (require 'work-init))))


;;; base.el ends here
