;;; judy-defaults.el --- Setting built-in defaults -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; General stuffs
;; Default to home
(setq default-directory "~/")

;; Answering just 'y' or 'n' will do
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (setq
;;  ;; ====== Default behavior ======
;;  ;; Inhibit startup message
;;  inhibit-splash-screen t
;;  inhibit-startup-message t ; default nil
;;  ;; Always prompt in minibuffer (no GUI)
;;  use-dialog-box nil
;;  ;; Use y or n instead of yes or no
;;  use-short-answers t
;;  ;; Confirm before quitting
;;  confirm-kill-emacs 'y-or-n-p
;;  )

(customize-set-variable 'inhibit-splash-screen t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'use-short-answers t)
(customize-set-variable 'confirm-kill-emacs 'y-or-n-p)

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

;; Clean up Files on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Make scripts runnable on save
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Default editing
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default display-line-numbers-width-start t)
;; (setq-default evil-shift-width tab-width)

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

;;;; CJK Word Wrap

;; Emacs 28 adds better word wrap / line break support for CJK.
(setq word-wrap-by-category t) ; default nil

;;;; default value

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; automatically revert buffers for changed files
(setq auto-revert-interval 5) ; default 5

;; 시간 표시 형식은 영어로 표시해서 호환성을 높입니다.
(setq system-time-locale "C")

;; Disable .# lock files
(setq create-lockfiles nil)

;; Shr group: Simple HTML Renderer 를 의미한다. 여기 설정을 바꾸면 faces 를 수정할 수 있음
(setq shr-use-fonts nil)

;; buffer size 를 표기 합니다.
(setq size-indication-mode t)

;; (global-visual-line-mode +1)

;; (tooltip-mode -1)           ; Disable tooltips
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar


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

;;; gui and terminal

;; (tooltip-mode -1)           ; Disable tooltips
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar

(when (display-graphic-p) ; gui

  ;; Read 'The Forgotten History of the Blinking Cursor'
  (blink-cursor-mode 1)
  (set-fringe-mode 10) ;; Give some breathing room
  (tooltip-mode 1)
  (global-display-fill-column-indicator-mode)

  ;; For my mouse that also has left - right mouse scroll
  (setq mouse-wheel-tilt-scroll t)
  (setq mouse-wheel-scroll-amount-horizontal 2)
  (setq mouse-wheel-flip-direction nil) ; default nil
  )

(unless (display-graphic-p) ; terminal
  (blink-cursor-mode -1)
  (gpm-mouse-mode -1)
  (xterm-mouse-mode -1)
  (mouse-wheel-mode -1)
  (pixel-scroll-precision-mode -1)

  (setq all-the-icons-color-icons nil)

  (setq mouse-wheel-follow-mouse -1)
  (setq
   mouse-drag-and-drop-region nil
   mouse-drag-and-drop-region-cross-program nil
   auto-window-vscroll nil
   fast-but-imprecise-scrolling nil
   scroll-preserve-screen-position nil
   pixel-scroll-precision-use-momentum nil
   )

  (setq evil-motions nil)

  (unless *is-termux*
    (require 'xclip)
    (xclip-mode 1))

  (require 'term-keys)
  (term-keys-mode t)
  )

;;; _
(provide 'judy-defaults)
;;; judy-defaults.el ends here
