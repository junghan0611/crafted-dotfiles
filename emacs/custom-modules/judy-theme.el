;;; judy-theme.el --- Theme setup (Modus Themes) -*- lexical-binding: t; -*-

;;; Commentary:
;; Various UI settings and setting up Modus Theme.
;; Basically anything (built-in) UI to modify (not: fonts, transparency).

;;; Code:

(setq inhibit-compacting-font-caches t)

;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
(minibuffer-depth-indicate-mode 1)

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; Disable .# lock files
(setq create-lockfiles nil)

;; Shr group: Simple HTML Renderer 를 의미한다. 여기 설정을 바꾸면 faces 를 수정할 수 있음
(setq shr-use-fonts nil)

;; buffer size 를 표기 합니다.
(setq size-indication-mode t)

;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html
;;텍스트를 선택한 다음 그 위에 입력하면 해당 텍스트가 삭제되어야 합니다.
;;놀랍게도 기본 Emac 에서는 이 동작이 기본적으로 제공되지 않습니다. 명시적으로
;;활성화해야 합니다.
(setq delete-selection-mode t) ; default nil
;; (setq magit-save-repository-buffers 'dontask) ; default t

;; Show a message when garbage collection happens? Useful while tuning the GC
;; (setq garbage-collection-messages t)


;;; Base UI
(customize-set-variable 'initial-scratch-message nil)
(setq ring-bell-function 'ignore)

;;; Line Numbers

;; (defun my/disable-line-numbers ()
;;   "Disabling line numbers."
;;   (display-line-numbers-mode 0))

(column-number-mode)
(setq display-line-numbers-type 'relative)

;; (dolist (mode '(term-mode-hook
;;                 shell-mode-hook
;;                 eshell-mode-hook
;;                 vterm-mode-hook))
;;   (add-hook mode #'my/disable-line-numbers))

;; (add-hook 'org-mode-hook 'display-line-numbers-mode)
;; (add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; Time

(require 'time)
(setq display-time-format " | %a %e %b, %H:%M | ")
;; Covered by `display-time-format'
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(setq display-time-interval 30) ; default 60
(setq display-time-default-load-average nil)

;; NOTE 2022-09-21: For all those, I have implemented my own solution
;; that also shows the number of new items, although it depends on
;; notmuch: the `notmuch-indicator' package.
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)

;; World clock
(setq zoneinfo-style-world-list
      '(("America/Los_Angeles" "Los Angeles")
        ("America/Chicago" "Chicago")
        ("Brazil/Acre" "Rio Branco")
        ("America/New_York" "New York")
        ("Brazil/East" "Brasília")
        ("Europe/Lisbon" "Lisbon")
        ("Europe/Brussels" "Brussels")
        ("Europe/Athens" "Athens")
        ("Asia/Tbilisi" "Tbilisi")
        ("Asia/Yekaterinburg" "Yekaterinburg")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Seoul" "Seoul")
        ("Asia/Vladivostok" "Vladivostok")))

;; All of the following variables are for Emacs 28
(setq world-clock-list t)
(setq world-clock-time-format "%R %z  %A %d %B")
(setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
(setq world-clock-timer-enable t)
(setq world-clock-timer-second 60)

;;; Winum

(require 'winum)
(setq winum-scope                      'frame-local
      winum-auto-assign-0-to-minibuffer t
      winum-reverse-frame-list          nil
      winum-auto-setup-mode-line nil
      winum-ignored-buffers '(" *LV*" " *which-key*"))
(define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
(define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
(define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
(define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
(define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
(define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
(define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
(define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
(define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
(define-key winum-keymap (kbd "M-9") 'winum-select-window-9)

(define-key winum-keymap
            [remap winum-select-window-9] #'switch-to-minibuffer-window)
(define-key winum-keymap
            [remap winum-select-window-7] #'spacemacs/neotree-smart-focus)

(winum-mode 1)

;;; Modus Theme

;; default to dark-mode
(setq modus-theme-region '(bg-only))

(customize-set-variable 'modus-themes-italic-constructs t)
(customize-set-variable 'modus-themes-bold-constructs t)

;; Headings for related modes
(customize-set-variable 'modus-themes-headings
                        '((1 . (rainbow overline 1.0))
                          (2 . (rainbow 1.0))
                          (3 . (rainbow bold 1.0))
                          (t . (semilight 1.0))))
(setq modus-theme-scale-headings nil)

(customize-set-variable 'modus-themes-org-blocks 'gray-background)
(setq modus-mixed-fonts nil)

;; general source
(customize-set-variable 'modus-themes-syntax '(yellow-comments))
(customize-set-variable 'modus-themes-lang-checkers '(straight-underline background))
(customize-set-variable 'modus-themes-paren-match '(bold))

;; Modeline
;; (setq modus-theme-mode-line '(accented borderless (padding . 4) (height . 0.9)))

;; Load theme
(load-theme 'modus-operandi t)

;;; DONT Doom-modeline

(require 'doom-modeline)
(setq doom-modeline-time nil)
(setq doom-modeline-time-icon nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-battery nil)
(setq doom-modeline-height 30)
(setq doom-modeline-bar-width 10) ; = fringe-mode
(setq Info-breadcrumbs-in-mode-line-mode nil)
(setq doom-modeline-icon nil)

;; (setq doom-modeline-enable-word-count t)
(setq doom-modeline-repl t)
(setq doom-modeline-lsp t)
(setq doom-modeline-github t)
(setq doom-modeline-indent-info t)
(setq doom-modeline-hud t)

;; truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;; (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
(setq doom-modeline-buffer-file-name-style 'truncate-upto-root)

(remove-hook 'display-time-mode-hook #'doom-modeline-override-time)
(remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-time)

(doom-modeline-mode +1)

;;; keycast

(require 'keycast)
(setq keycast-tab-bar-minimal-width 50)
(setq keycast-tab-bar-format "%10s%k%c%r")

;; (when (string= (system-name)"jhnuc")
(unless *is-termux*
  (add-hook 'after-init-hook 'keycast-tab-bar-mode))

(dolist (input '(self-insert-command
                 org-self-insert-command))
  (add-to-list 'keycast-substitute-alist `(,input ">>>>>>>>" "Typing.....")))
;; (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

(dolist (event '(mouse-event-p
                 mouse-movement-p
                 mwheel-scroll

                 ;; 2023-10-02 Added for clojure-dev
                 lsp-ui-doc--handle-mouse-movement
                 ignore-preserving-kill-region
                 ;; mouse-set-region
                 ;; mouse-set-point
                 ))
  (add-to-list 'keycast-substitute-alist `(,event nil)))

;;; time
(defun my/load-global-mode-string ()
  (interactive)
  ;; (message "my/load-global-mode-string")
  (when (not (bound-and-true-p display-time-mode))
    (display-time-mode t))
  )

(unless *is-termux*
  (add-hook 'after-init-hook #'my/load-global-mode-string))

;;; nerd-icons

(unless *is-termux*
  (require 'nerd-icons-dired)
  (require 'nerd-icons-completion)

  (when (display-graphic-p) ; gui
    (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
    (nerd-icons-completion-mode))
  )

;;; neotree

(require 'neotree)

(setq neo-window-width 32
      neo-create-file-auto-open t
      neo-banner-message "Press ? for neotree help"
      neo-show-updir-line nil
      neo-mode-line-type 'neotree
      neo-smart-open t
      neo-dont-be-alone t
      neo-persist-show nil
      neo-show-hidden-files t
      neo-auto-indent-point t
      neo-modern-sidebar t
      neo-vc-integration nil)

;;; shackle

(require 'shackle)
(setq shackle-default-size 0.4
      shackle-rules `(
                      ;; (help-mode                       :select t :align right :size ,fill-column)
                      ;; (helpful-mode                    :select t :align right :size ,fill-column)

                      ;; select nil 일 때, 'q' 로 바로 닫을 수 있다.
                      (help-mode                       :select nil :align t)
                      (helpful-mode                    :select nil :align t)

                      ("*Messages*"                    :select nil :align t)
                      ("*eldoc*"                       :align t)
                      (special-mode                    :align t)
                      (process-menu-mode               :align t)
                      (compilation-mode                :align t)
                      (flymake-diagnostics-buffer-mode :align t)
                      ("*Shell Command Output*"        :align t)
                      ("*Async Shell Command*"         :align t)
                      ("\\*EGLOT.*"                    :select t :align right :size ,fill-column :regexp t)))

(add-hook 'after-init-hook 'shackle-mode)

;;; popper

(require 'popper)
(setq popper-echo-dispatch-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
(setq popper-display-control nil) ; use popwin and display-buffer-alist
(setq popper-reference-buffers
      '("\\*Messages\\*"
        ;; "Output\\*$"
        "*cider-error*"
        ;; "*cider-doc*"
        ;; "^\\*eldoc for"
        "\\*Async-native-compile-log\\*" ; JH
        "^\\*EGLOT" ; JH
        "^\\*Flycheck.+\\*$" ; JH
        ;; treemacs-mode ; JH
        "*Go-Translate*" ; JH
        "*wordreference*" ; JH
        "*tmr-tabulated-view*" ; JH
        "*SDCV*" ; JH
        "*Dogears List*" ; JH
        "^\\*Backtrace\\*"
        "*Hammy Log*"
        ;; "*eww*"
        "*lsp-documentation*"
        "*devdocs-javascript*"
        ;; zk-index-mode
        help-mode
        telega-chat-mode
        helpful-mode
        compilation-mode
        process-menu-mode
        special-mode
        eww-mode
        ;; "*Emacs Log*"
        ;; "*command-log*" ; JH
        flymake-diagnostics-buffer-mode))
(add-to-list
 'popper-reference-buffers
 '(("^\\*Warnings\\*$" . hide)
   ("^\\*Compile-Log\\*$" . hide)
   "^\\*Matlab Help.*\\*$"
   "^\\*Messages\\*$"
   ("*typst-ts-compilation*" . hide)
   ("^\\*dash-docs-errors\\*$" . hide)
   "^\\*evil-registers\\*"
   "^\\*Apropos"
   "^Calc:"
   "^\\*eldoc\\*"
   "^\\*TeX errors\\*"
   "^\\*ielm\\*"
   "^\\*TeX Help\\*"
   "^\\*ChatGPT\\*"
   "^\\*gptel-quick\\*"
   "^\\*define-it:"
   "\\*Shell Command Output\\*"
   "\\*marginal notes\\*"
   ("\\*Async Shell Command\\*" . hide)
   "\\*Completions\\*"
   "[Oo]utput\\*"))

;; (global-set-key (kbd "C-`") 'popper-toggle)
;; (global-set-key (kbd "C-~") 'popper-kill-latest-popup)
;; (global-set-key (kbd "M-`") 'popper-cycle)
;; (global-set-key (kbd "C-M-`") 'popper-toggle-type)
(popper-mode +1)
(popper-echo-mode +1)

;;; _
(provide 'judy-theme)
;;; judy-theme.el ends here
