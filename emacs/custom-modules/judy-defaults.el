;;; judy-defaults.el --- Setting built-in defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; General stuffs
;; Default to home
(setq default-directory "~/")

;; Disable warnings
(customize-set-variable 'byte-compile-warnings '(cl-functions))
(customize-set-variable 'large-file-warning-threshold nil)
(customize-set-variable 'vc-follow-symlinks t)

;; Auto-revert buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;; Single dired-buffer
(customize-set-variable 'dired-kill-when-opening-new-dired-buffer t)

;;; Editing
;; Helping with visual things
(global-prettify-symbols-mode t)
(global-hl-line-mode)

;; 80 cols inidicator
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode)

;; Clean up Files on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Make scripts runnable on save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Default editing
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; prefer UTF-8 and english
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "English")

;;; No Littering
;; banish customized variables
(setq custom-file
      (expand-file-name ".cache/custom-vars.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;; setup backups of files
(setq backup-directory-alist
      `(("." . ,(expand-file-name ".cache/backups" user-emacs-directory)))
      make-backup-files t
      vc-make-backup-files t
      version-control t
      delete-old-versions t
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      delete-by-moving-to-trash t
      backup-by-copying t)

;; setup recovery
(setq auto-save-list-file-prefix
      (expand-file-name ".cache/auto-save-list/.saves-" user-emacs-directory)
      auto-save-default t
      auto-save-timeout 30
      auto-save-interval 300)

;; setup bookmarks file
(setq bookmark-default-file
      (expand-file-name ".cache/booksmark" user-emacs-directory))

;;; _
(provide 'judy-defaults)
;;; judy-defaults.el ends here
