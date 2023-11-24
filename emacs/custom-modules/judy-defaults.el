;;; judy-defaults.el --- Setting built-in defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; General stuffs
;; Default to home
(setq default-directory "~/")

;; 불필요한 Package cl is deprecated 경고 숨기기
(customize-set-variable 'byte-compile-warnings '(not cl-functions))
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
;; (setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(setq-default display-line-numbers-width-start t)

;;; Hangul Korean

;; prefer UTF-8 and english

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system  'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default line-spacing 3) ; use fontaine

;; (setenv "LANG" "en_US.UTF-8")
;; (setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LANG" "ko_KR.UTF-8")

;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.
(setq system-time-locale "C")

(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

(global-set-key (kbd "<S-SPC>") 'toggle-input-method)
;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
;; (global-unset-key (kbd "S-SPC"))

;; 입력 모드에서만 한영 변환 가능!
(defun my/turn-off-input-method (&rest _)
  (if current-input-method
      (deactivate-input-method)))

(advice-add 'evil-normal-state :before #'my/turn-off-input-method)
(mapc (lambda (mode)
        (let ((keymap (intern (format "evil-%s-state-map" mode))))
          (define-key (symbol-value keymap) [?\S- ]
                      #'(lambda () (interactive)
                          (message
                           (format "Input method is disabled in %s state." evil-state))))))
      '(motion normal visual))

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

;;;; dired

(require 'dired)
(require 'dired-x)

(setq
 ;; dired-listing-switches "-laFGh1v --group-directories-first"
 dired-ls-F-marks-symlinks t ; -F marks links with @
 ;; Inhibit prompts for simple recursive operations
 dired-recursive-copies 'always
 ;; Auto-copy to other Dired split window
 dired-dwim-target t

 ;; dired-auto-revert-buffer t ; Revert on re-visiting
 ;; (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
 ;; (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
 dired-auto-revert-buffer t

 dired-listing-switches
 "-AGFhlv --group-directories-first --time-style=long-iso"

 dired-kill-when-opening-new-dired-buffer t
 dired-make-directory-clickable t ; Emacs 29.1
 dired-mouse-drag-files t) ; Emacs 29.1

;; (setq dired-recursive-deletes 'always)
;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; (setq copy-directory-create-symlink t)
;; (setq dired-hide-details-hide-symlink-targets nil) ; default t

;; In Emacs 29 there is a binding for `repeat-mode' which let you
;; repeat C-x C-j just by following it up with j.  For me, this is a
;; problem as j calls `dired-goto-file', which I often use.
;; (define-key dired-jump-map (kbd "j") nil)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (setq-local truncate-lines t) ; Do not wrap lines
            ;; (visual-line-mode -1)
            (hl-line-mode 1)))

;; wdired is a mode that allows you to rename files and directories by editing the
(require 'wdired)

(setq wdired-allow-to-change-permissions t
      wdired-create-parent-directories t)
(evil-define-key 'normal wdired-mode-map (kbd "^") 'evil-first-non-blank)
(evil-define-key 'normal dired-mode-map
  (kbd "C-c C-e") 'wdired-change-to-wdired-mode
  (kbd ".") 'consult-line
  (kbd "S-SPC") 'dired-toggle-marks
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file)

;;; _
(provide 'judy-defaults)
;;; judy-defaults.el ends here
