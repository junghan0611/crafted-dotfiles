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

(customize-set-variable 'tab-always-indent t)

(customize-set-variable 'inhibit-splash-screen t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'use-short-answers t)
(customize-set-variable 'confirm-kill-emacs 'y-or-n-p)

;; 불필요한 Package cl is deprecated 경고 숨기기
(customize-set-variable 'byte-compile-warnings '(not cl-functions))
(customize-set-variable 'large-file-warning-threshold nil)
(customize-set-variable 'vc-follow-symlinks t)

(customize-set-variable 'enable-recursive-minibuffers t)

;; Auto-revert buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;; Single dired-buffer
(customize-set-variable 'dired-kill-when-opening-new-dired-buffer t)

;;; spacemacs overide

(setq
 ;; ====== Default behavior ======
 ;; Inhibit startup message
 inhibit-splash-screen t
 inhibit-startup-message t ; default nil
 ;; Do not ring
 ;; ring-bell-function 'ignore
 ;; Increase the large file threshold to 50 MiB
 large-file-warning-threshold (* 50 1024 1024)

 ;; Initial scratch message (will be overridden if "fortune" is installed)
 ;; initial-scratch-message ";; MinEmacs -- start here!"
 ;; Set initial buffer to fundamental-mode for faster load
 ;; initial-major-mode 'fundamental-mode

 ;; Always prompt in minibuffer (no GUI)
 use-dialog-box nil
 ;; Use y or n instead of yes or no
 use-short-answers t
 ;; Confirm before quitting
 confirm-kill-emacs 'y-or-n-p
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t

 ;; Save files only in sub-directories of current project
 ;; save-some-buffers-default-predicate 'save-some-buffers-root

 ;; Use single space between sentences
 sentence-end-double-space nil
 ;; Move stuff to trash
 delete-by-moving-to-trash t
 ;; trash-directory "~/.Trash"

 ;; Select help window for faster quit!
 help-window-select t

 ;; FIXME More info on completions
 completions-detailed t

 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t

 ;; Kill the shell buffer after exit
 shell-kill-buffer-on-exit t

 ;; ====== Passwords and encryption ======
 ;; Default auth-sources to GPG
 auth-sources '("~/.authinfo.gpg")
 ;; Enable password caching
 password-cache t
 ;; 10 minutes, default is 16 sec
 password-cache-expiry 600
 ;; Enable caching, do not keep asking about GPG key
 auth-source-do-cache t
 ;; All day, default is 2h (7200)
 auth-source-cache-expiry 86400

 ;; ====== Performances ======
 ;; Don’t compact font caches during GC
 inhibit-compacting-font-caches t
 ;; Increase single chunk bytes to read from subprocess (default 4096)
 read-process-output-max (if (eq system-type 'gnu/linux)
                             (condition-case nil
                                 ;; Android may raise permission-denied error
                                 (with-temp-buffer
                                   (insert-file-contents
                                    "/proc/sys/fs/pipe-max-size")
                                   (string-to-number (buffer-string)))
                               ;; If an error occured, fallback to the default value
                               (error read-process-output-max))
                           (* 1024 1024))

 ;; TODO 2023-06-19 왜 갑자기 클라이언트 프레임 사이즈가 이상하지?!
 ;; Do force frame size to be a multiple of char size
 frame-resize-pixelwise t

 ;; ;; Emacsclient does not use full frame size (NIL 필수!)
 frame-inhibit-implied-resize nil

 ;; Stretch cursor to the glyph width
 ;; make cursor the width of the character it is under
 ;; i.e. full width of a TAB
 x-stretch-cursor t
 ;; Show trailing whitespaces
 show-trailing-whitespace t
 ;; Resize window combinations proportionally
 window-combination-resize t
 ;; Enable time in the mode-line
 ;; display-time-string-forms '((propertize (concat 24-hours ":" minutes)))
 ;; No ugly button for widgets
 widget-image-enable nil
 ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
 ;; prettify-symbols-unprettify-at-point t
 ;; Make tooltips last a bit longer (default 10s)
 tooltip-hide-delay 20
 ;; Use small frames to display tooltips instead of the default OS tooltips
 use-system-tooltips nil

 ;; ====== Undo ======
 ;; 10MB (default is 160kB)
 undo-limit 10000000
 ;; 50MB (default is 240kB)
 undo-strong-limit 50000000
 ;; 150MB (default is 24MB)
 undo-outer-limit 150000000

 ;; ====== Editing ======
 ;; Default behavior for `whitespace-cleanup'
 ;; whitespace-action '(cleanup auto-cleanup)
 ;; End files with newline
 require-final-newline t

 ;; Enable Drag-and-Drop of regions
 mouse-drag-copy-region t
 mouse-drag-and-drop-region t
 ;; Enable Drag-and-Drop of regions from Emacs to external programs
 mouse-drag-and-drop-region-cross-program t

 ;; ====== Scrolling ======
 ;; Do not adjust window-vscroll to view tall lines
 auto-window-vscroll nil
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t
 ;; Do not move cursor to the center when scrolling
 ;; scroll-conservatively 101
 ;; Scroll at a margin of one line
 scroll-margin 1
 ;; Better scrolling on Emacs29+, specially on a touchpad
 pixel-scroll-precision-use-momentum t

 column-number-mode t ; default nil

 ;; 복붙만 한다.
 ;; ;; ====== Compilation ======
 ;; ;; Scroll compilation buffer
 ;; compilation-scroll-output t ; 'first-error can be a good option
 ;; ;; Always kill current compilation process before starting a new one
 ;; compilation-always-kill t
 ;; ;; Skip visited messages on compilation motion commands
 ;; compilation-skip-visited t
 ;; ;; Keep it readable
 ;; compilation-window-height 12
 )
;; Kill minibuffer when switching by mouse to another window
;; Taken from: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
;; (add-hook
;;  'mouse-leave-buffer-hook
;;  (defun +minibuffer--kill-on-mouse-h ()
;;    "Kill the minibuffer when switching to window with mouse."
;;    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
;;      (abort-recursive-edit))))

;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
(pixel-scroll-precision-mode 1)

;; Files with known long lines
;; SPC f l to open files literally to disable most text processing
;; So long mode when Emacs thinks a file would affect performance
(global-so-long-mode 1)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Emacs text rendering optimizations
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
;; Only render text left to right
(setq-default bidi-paragraph-direction 'left-to-right)

;; 라인 컬럼 보여주는 검은 세로선
(when (display-graphic-p) ; gui
  (global-display-fill-column-indicator-mode))

;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html
;;텍스트를 선택한 다음 그 위에 입력하면 해당 텍스트가 삭제되어야 합니다.
;;놀랍게도 기본 Emac 에서는 이 동작이 기본적으로 제공되지 않습니다. 명시적으로
;;활성화해야 합니다.
(setq delete-selection-mode t) ; default nil
;; (setq magit-save-repository-buffers 'dontask) ; default t

;; Show a message when garbage collection happens? Useful while tuning the GC
;; (setq garbage-collection-messages t)
;; (add-function :after
;;               after-focus-change-function
;;               (lambda () (unless (frame-focus-state)
;;                            (garbage-collect))))

;; ====== Recent files ======
;; Increase the maximum number of saved items
;; Ignore case when searching recentf files
(setq recentf-case-fold-search t)
;; Exclude some files from being remembered by recentf
(setq recentf-max-saved-items 200) ; default 20

;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
(minibuffer-depth-indicate-mode 1)

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
(setq-default tab-width 4)
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
;; (setq backup-directory-alist
;;       `(("." . ,(expand-file-name ".cache/backups" user-emacs-directory)))
;;       make-backup-files t
;;       vc-make-backup-files t
;;       version-control t
;;       delete-old-versions t
;;       kept-old-versions 0
;;       kept-new-versions 10
;;       delete-old-versions t
;;       delete-by-moving-to-trash t
;;       backup-by-copying t)

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

(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar

(unless *is-termux*
  (scroll-bar-mode -1))

(when (display-graphic-p) ; gui
  ;; Read 'The Forgotten History of the Blinking Cursor'
  (blink-cursor-mode 1)
  (set-fringe-mode 10) ;; Give some breathing room
  (tooltip-mode 1)
  ;; (global-display-fill-column-indicator-mode)

  (setq mouse-wheel-tilt-scroll nil)
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
