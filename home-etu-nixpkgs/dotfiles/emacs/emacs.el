;;; emacs.el -- starts here


;;; Commentary:
;; This is the init-file for my Emacs config ;)


;;; Code:
;; Define XDG directories
(defvar user-emacs-config-directory
  (concat (getenv "HOME") "/.config/emacs"))

(defvar user-emacs-data-directory
  (concat (getenv "HOME") "/.local/share/emacs"))

(defvar user-emacs-cache-directory
  (concat (getenv "HOME") "/.cache/emacs"))

;; Increase the threshold to reduce the amount of garbage collections made
;; during startups.
(let ((gc-cons-threshold (* 50 1000 1000))
      (gc-cons-percentage 0.6))

  ;; Init package manager
  (package-initialize)


  ;; Avoid creation of ~/.emacs.d/
  (setq-default user-emacs-directory user-emacs-data-directory)


  ;; Load config
  (org-babel-load-file (concat user-emacs-config-directory "/config.org")))


;;; emacs.el ends here
