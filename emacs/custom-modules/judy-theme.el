;;; judy-theme.el --- Theme setup (Modus Themes) -*- lexical-binding: t; -*-

;;; Commentary:
;; Various UI settings and setting up Modus Theme.
;; Basically anything (built-in) UI to modify (not: fonts, transparency).

;;; Code:

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
(setq garbage-collection-messages t)


;;; Base UI
(customize-set-variable 'initial-scratch-message nil)
(scroll-bar-mode -1)   ;; Disable visible scrollbar
(tool-bar-mode -1)     ;; Disable toolbar
(tooltip-mode 1)      ;; Enable tooltips
(menu-bar-mode 1)     ;; Enable menubar
(set-fringe-mode 10)   ;; "breathing room"
(setq ring-bell-function 'ignore)

;;; Line Numbers
(defun my/disable-line-numbers ()
  "Disabling line numbers."
  (display-line-numbers-mode 0))

(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode #'my/disable-line-numbers))

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
(winum-mode 1)

;;; Doom-modeline

(require 'doom-modeline)
(setq doom-modeline-time nil)
(setq doom-modeline-time-icon nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-battery nil)
(setq doom-modeline-height 30)
(setq doom-modeline-bar-width 10) ; = fringe-mode
(setq Info-breadcrumbs-in-mode-line-mode nil)

(setq doom-modeline-window-width-limit (- fill-column 10))

;; (unless (display-graphic-p) ; terminal
;;   (setq doom-modeline-icon nil)) ; important
(setq doom-modeline-icon nil)

(setq doom-modeline-enable-word-count t)
(setq doom-modeline-repl t)
(setq doom-modeline-lsp t)
(setq doom-modeline-github t)
(setq doom-modeline-indent-info t)
(setq doom-modeline-hud t)
(setq doom-modeline-env-python-executable "python")
;; (setq doom-modeline-window-width-limit nil)

;; truncate-upto-project => ~/P/F/emacs/lisp/comint.el
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; (setq doom-modeline-env-python-executable "python")
;; (setq doom-modeline-window-width-limit nil)
;; (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(remove-hook 'display-time-mode-hook #'doom-modeline-override-time)
(remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-time)

(doom-modeline-mode 1)

;;; modus-themes
(require 'modus-themes)
;; Load the theme of your choice.
(setq modus-themes-to-toggle (let ((hr (nth 2 (decode-time))))
                               (if (or (< hr 7) (< 19 hr))           ; between 8 PM and 7 AM
                                   '(modus-vivendi-tinted modus-operandi-tinted) ; load dark theme first
                                 '(modus-operandi-tinted modus-vivendi-tinted))))

(setq modus-themes-org-blocks 'gray-background)

(setq modus-themes-italic-constructs nil
      modus-themes-bold-constructs t
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t ; default t

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `italic', `WEIGHT'
      ;; modus-themes-prompts '(bold)

      ;; The `modus-themes-completions' is an alist that reads two
      ;; keys: `matches', `selection'.  Each accepts a nil value (or
      ;; empty list) or a list of properties that can include any of
      ;; the following (for WEIGHT read further below):
      ;; `matches'   :: `underline', `italic', `WEIGHT'
      ;; `selection' :: `underline', `italic', `WEIGHT'
      ;; modus-themes-completions
      ;; '((matches   . (semibold))
      ;;   (selection . (semibold text-also)))
      )

;; 'M-x' modus-themes-preview-colors-current
(setq modus-themes-common-palette-overrides
      `(
        ;; Customize the mode-line colors
        (fg-mode-line-active fg-main) ; Black
        ;; (bg-mode-line-active bg-blue-intense)

        ;; "Make the mode line borderless"
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)

        ;; "Make matching parenthesis more or less intense"
        ;; (bg-paren-match bg-magenta-intense)
        ;; (underline-paren-match unspecified)

        ;; Links
        ;; (underline-link border)
        ;; (underline-link-visited border)
        ;; (underline-link-symbolic border)

        ;; Comments are yellow, strings are green
        (comment yellow-cooler)
        (string green-warmer)

        ;; Intense magenta background combined with the main foreground
        ;; (bg-region bg-magenta-subtle)
        ;; (fg-region fg-main)

        ;; (bg-heading-0 bg-green-nuanced) ; green
        ;; (bg-heading-1 bg-dim) ; white
        ;; (bg-heading-2 bg-yellow-nuanced) ; yellow
        ;; (bg-heading-3 bg-blue-nuanced) ; blue
        ;; (bg-heading-4 bg-magenta-nuanced) ; magenta
        ;; (bg-heading-5 bg-cyan-nuanced) ; cyan

        ;; And expand the preset here. Note that the ,@ works because we use
        ;; the backtick for this list, instead of a straight quote.
        ;; 현재 설정에 faint, intense 컬러 세트를 덮어쓰고 싶다면
        ;; ,@modus-themes-preset-overrides-faint
        ;; ,@modus-themes-preset-overrides-intense
        )
      )

(when (display-graphic-p) ; gui
  ;; Users may need to explicitly configure the font family of
  ;; fixed-pitch in order to get a consistent experience with their
  ;; typography (also check the fontaine package on GNU ELPA (by
  ;; Protesilaos)).
  (setq modus-themes-mixed-fonts nil)

  ;; In all of the following, WEIGHT is a symbol such as `semibold',
  ;; `light', `bold', or anything mentioned in `modus-themes-weights'.
  (setq modus-themes-variable-pitch-ui t)

  ;; The `modus-themes-headings' is an alist: read the manual's
  ;; node about it or its doc string. Basically, it supports
  ;; per-level configurations for the optional use of
  ;; `variable-pitch' typography, a height value as a multiple of
  ;; the base font size (e.g. 1.5), and a `WEIGHT'.
  (setq modus-themes-headings
        '(
          (0                . (variable-pitch bold 1.2))
          (1                . (variable-pitch bold 1.1))
          (2                . (variable-pitch semibold 1.05))
          (3                . (variable-pitch semibold 1.0))
          (4                . (variable-pitch medium 1.0))
          (5                . (variable-pitch medium 1.0))
          (6                . (variable-pitch medium 1.0))
          (agenda-date      . (variable-pitch semibold 1.2))
          (agenda-structure . (variable-pitch semibold 1.1))
          (t                . (variable-pitch medium 1.0))))
  ) ; end-of gui-mode

(defun my-modus-themes-fixed-pitch-colors ()
  (modus-themes-with-colors
    (custom-set-faces
     `(org-property-value ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata-value)))
     `(org-drawer ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
     `(org-tag ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-tag)))
     ;; `(org-sexp-date ((,c :inherit modus-themes-fixed-pitch :foreground ,date-common :height 0.9)))

     `(org-document-info ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata-value)))
     `(org-document-info-keyword ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
     `(org-meta-line ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))

     `(org-block ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-main :background ,bg-dim)))
     `(org-block-begin-line ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-block :background ,bg-inactive)))
     `(org-block-end-line ((,c :inherit org-block-begin-line)))

     `(org-date ((,c :inherit modus-themes-fixed-pitch :foreground ,date-common)))
     `(org-date-selected ((,c :inherit modus-themes-fixed-pitch :foreground ,date-common :inverse-video t)))
     `(org-table ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table)))
     `(org-formula ((,c :inherit modus-themes-fixed-pitch :foreground ,fnname)))
     `(org-hide ((,c :inherit modus-themes-fixed-pitch :foreground ,bg-main)))
     )
    )
  )

;; Modus Toggle 로 불러올 때 아래 Hook 이 호출 된다.
(defun my-modus-themes-colors ()
  (modus-themes-with-colors
    (custom-set-faces
     `(fringe ((,c :background ,bg-dim)))
     `(vterm-color-black ((,c :background "gray25" :foreground "gray25")))
     `(vterm-color-yellow ((,c :background ,yellow-intense :foreground ,yellow-intense)))
     `(translate-paragraph-highlight-face ((,c :extend t :background ,bg-red-subtle)))
     `(tab-bar ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-tab-bar :weight semibold)))
     `(tab-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-tab-bar :weight semibold))) ; :height 1.0
     `(jinx-misspelled ((,c :underline (:style wave :color ,magenta-intense))))
     `(treemacs-root-face ((,c :inherit org-level-2 :underline nil :weight bold :height 1.0)))
     `(treemacs-directory-face ((,c :inherit org-level-3 :height 1.0)))
     `(treemacs-file-face ((,c :inherit org-level-4 :weight regular :height 1.0)))

     ;; `(line-number ((,c :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default) :background ,bg-line-number-inactive :foreground ,fg-line-number-inactive :height 0.9)))
     ;; `(line-number-current-line ((,c :inherit (bold line-number) :background ,bg-line-number-active :foreground ,fg-line-number-active :height 0.9)))

     `(imenu-list-entry-face-0 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight bold :foreground ,fg-heading-1)))
     `(imenu-list-entry-subalist-face-0 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight bold :foreground ,fg-heading-1 :underline nil)))
     `(imenu-list-entry-face-1 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight semibold :foreground ,fg-heading-2)))
     `(imenu-list-entry-subalist-face-1 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight semibold :foreground ,fg-heading-2 :underline nil)))
     `(imenu-list-entry-face-2 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight semibold :foreground ,fg-heading-3)))
     `(imenu-list-entry-subalist-face-2 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight semibold :foreground ,fg-heading-3 :underline nil)))
     `(imenu-list-entry-face-3 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight semibold :foreground ,fg-heading-4)))
     `(imenu-list-entry-subalist-face-3 ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight semibold :foreground ,fg-heading-4 :underline nil)))

     `(org-side-tree-heading-face ((,c :inherit modus-themes-ui-variable-pitch :width narrow :weight bold :foreground ,fg-heading-1 :underline nil)))

     `(highlight-indentation-current-column-face ((,c :background ,fg-alt))) ; fg-heading-0

     `(doom-modeline-input-method ((,c :weight regular :foreground ,bg-main :background ,red-cooler)))
     `(doom-modeline-evil-insert-state ((,c :weight regular :foreground ,bg-main :background ,green-cooler)))
     `(doom-modeline-evil-visual-state ((,c :weight regular :foreground ,bg-main :background ,yellow-cooler)))
     `(doom-modeline-evil-motion-state ((,c :weight regular :foreground ,bg-main :background ,blue-cooler)))
     `(doom-modeline-evil-emacs-state ((,c :weight regular :foreground ,bg-main :background ,cyan-cooler)))
     `(doom-modeline-evil-replace-state ((,c :weight regular :foreground ,bg-main :background ,magenta-cooler)))
     ;; `(doom-modeline-evil-normal-state (( )))
     ;; `(doom-modeline-evil-operator-state ((,c :inherit bold)))

     `(lsp-ui-doc-background ((,c (:background ,bg-dim))))
     `(lsp-ui-doc-header ((,c (:foreground ,fg-main :background ,bg-active :height 1.1))))
     `(lsp-ui-doc-url ((,c (:foreground ,fg-alt))))
     `(lsp-ui-sideline-code-action ((,c (:foreground ,fg-mark-select))))

     ;; org-mode
     `(org-level-2 ((,c :inherit modus-themes-heading-2 :underline t)))
     `(org-mode-line-clock ((,c :inherit bold :foreground ,modeline-info)))
     `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,modeline-err)))
     )
    )
  )

(add-hook 'modus-themes-after-load-theme-hook
          (lambda ()
            (my-modus-themes-colors)
            (my-modus-themes-fixed-pitch-colors)
            ))

;;; ef-themes

(require 'ef-themes)

;; Read the doc string or manual for this one.  The symbols can be
;; combined in any order.
;; (setq ef-themes-region '(intense no-extend neutral))

(when (display-graphic-p) ; gui
  (setq ef-themes-mixed-fonts nil)
  (setq ef-themes-variable-pitch-ui t)
  (setq ef-themes-headings
        '(
          (0                . (variable-pitch bold 1.2))
          (1                . (variable-pitch bold 1.0))
          (2                . (variable-pitch semibold 1.0))
          (3                . (variable-pitch semibold 1.0))
          (4                . (variable-pitch medium 1.0))
          (5                . (variable-pitch medium 1.0))
          (6                . (variable-pitch medium 1.0))
          (7                . (variable-pitch medium 1.0))
          (8                . (variable-pitch medium 1.0))
          (agenda-date      . (variable-pitch semibold 1.1))
          (agenda-structure . (variable-pitch semibold 1.1))
          (t                . (variable-pitch medium 1.0))))
  ) ; end-of gui

(defun my-ef-themes-mode-line ()
  "Tweak the style of the mode lines."
  (ef-themes-with-colors
    (custom-set-faces
     `(fringe ((,c :background ,bg-dim)))
     `(tab-bar ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-tab-bar :weight semibold)))
     `(tab-line ((,c :inherit ef-themes-ui-variable-pitch :background ,bg-tab-bar :weight semibold))) ; :height 1.0
     `(org-mode-line-clock ((,c :inherit bold :foreground ,modeline-info)))
     `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,modeline-err)))
     )))
(add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line)

(setq ef-themes-to-toggle (let ((hr (nth 2 (decode-time))))
                            (if (or (< hr 7) (< 19 hr))           ; between 8 PM and 7 AM
                                '(ef-maris-dark ef-trio-light) ; load dark theme first
                              '(ef-trio-light ef-maris-dark))))

;;; keycast

(require 'keycast)
(setq keycast-tab-bar-minimal-width 50)
(setq keycast-tab-bar-format "%10s%k%c%r")

;; (add-hook 'after-init-hook 'keycast-tab-bar-mode)

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

;;; tab-bar

(require 'tab-bar)
(setq auto-resize-tab-bars t) ; important
(setq tab-bar-select-tab-modifiers '(control meta))
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-close-button-show nil)
(setq tab-bar-close-last-tab-choice nil)
(setq tab-bar-close-tab-select 'recent)
(setq tab-bar-new-tab-to 'right)
(setq tab-bar-position nil)
(setq tab-bar-show nil)
(setq tab-bar-tab-hints t) ; for tab-bar-circle-number

(setq tab-bar-tab-name-function 'tab-bar-tab-name-current) ; default

(setq tab-bar-format                    ; Emacs 28
      '(
        tab-bar-separator
        tab-bar-format-menu-bar
        ;; tab-bar-format-tabs
        tab-bar-format-tabs-groups
        tab-bar-separator
        tab-bar-format-add-tab

        tab-bar-format-align-right
        tab-bar-format-global
        ))

(defun my/reload-tab-bar ()
  (interactive)

  (keycast-tab-bar-mode -1)
  (tab-bar-history-mode -1)
  (setq tab-bar-show nil)
  (tab-bar-mode -1)

  (setq tab-bar-show t)
  (tab-bar-history-mode 1)

  (tab-bar-mode 1)
  (keycast-tab-bar-mode 1)
  )

;; explicitly re-enable the cat for the first GUI client
;; 순서 상으로 먼저 탭바를 로드하고 테마를 로딩하는게 맞다.
(add-hook 'after-init-hook #'my/reload-tab-bar)

;;;; time
(defun my/load-global-mode-string ()
  (interactive)
  ;; (message "my/load-global-mode-string")
  (when (not (bound-and-true-p display-time-mode))
    (display-time-mode t))
  )

(add-hook 'after-init-hook #'my/load-global-mode-string)

;; Load-theme
;; (ef-themes-toggle)
;; (modus-themes-toggle)
(add-hook 'after-init-hook #'modus-themes-toggle)


;;; _
(provide 'judy-theme)
;;; judy-theme.el ends here
