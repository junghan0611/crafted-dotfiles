;;; judy-keys.el --- Keyboard config -*- lexical-binding: t; -*-
;;; Commentary:

;; Keyboard and shortcuts

;;; Code:

;;;; TODO Move
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(progn
  (global-set-key (kbd "C-s-h") 'tab-previous)
  (global-set-key (kbd "C-<backtab>") 'tab-previous)
  (global-set-key (kbd "C-s-l") 'tab-next)
  (global-set-key (kbd "s-\\") 'tab-bar-switch-to-tab)
  (global-set-key (kbd "C-s-1") #'(lambda() (interactive) (tab-bar-select-tab 1)))
  (global-set-key (kbd "C-s-2") #'(lambda() (interactive) (tab-bar-select-tab 2)))
  (global-set-key (kbd "C-s-3") #'(lambda() (interactive) (tab-bar-select-tab 3)))
  (global-set-key (kbd "C-s-4") #'(lambda() (interactive) (tab-bar-select-tab 4)))
  (global-set-key (kbd "C-s-5") #'(lambda() (interactive) (tab-bar-select-tab 5)))
  (global-set-key (kbd "C-s-6") #'(lambda() (interactive) (tab-bar-select-tab 6)))
  (global-set-key (kbd "C-s-7") #'(lambda() (interactive) (tab-bar-select-tab 7)))
  (global-set-key (kbd "M-s-l") 'evil-window-right)
  (global-set-key (kbd "M-s-h") 'evil-window-left)
  (global-set-key (kbd "M-s-k") 'evil-window-up)
  (global-set-key (kbd "M-s-j") 'evil-window-down)
  )

;; If you use a window manager be careful of possible key binding clashes
(global-set-key (kbd "M-<tab>") 'other-window) ; very useful
(global-set-key (kbd "M-<iso-lefttab>") (lambda() (interactive) (other-window -1))) ; == M-S-<tab>
(global-set-key (kbd "M-<backtab>") (lambda() (interactive) (other-window -1))) ; for terminal

(global-set-key (kbd "<f7>") 'neotree-toggle)
(global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
(global-set-key (kbd "M-y") 'consult-yank-pop)

;;;; jump out / in parens

;; Keybindings
;; 자동 완성 하지 않고 다음 줄 - C-<return>
;; 자동 완성 하지 않고 괄호 점프 - Tab
;; 자동 완성 하지 않고 현재 위치 - C-q : corfu-quit
;; 자동 완성 하지 않고 다음 위치 - Space
;; 자동 완성 - <return>

;;   ;; Tab 이 자동 완성이면 괄호 점프랑 충돌 난다.
;;   ;; C-j/k C-n/p는 직관적인 기본 설정이므로 건들이지 않는다.
(with-eval-after-load 'corfu
  (evil-define-key '(insert) org-mode-map (kbd "C-M-<return>") 'jump-out-of-pair)
  (evil-define-key '(insert) prog-mode-map (kbd "C-M-<return>") 'jump-out-of-pair)

  (evil-define-key '(insert) prog-mode-map (kbd "<tab>") 'jump-out-of-pair)
  (evil-define-key '(insert) prog-mode-map (kbd "TAB") 'jump-out-of-pair)
  (evil-define-key '(insert) corfu-map (kbd "<tab>") 'jump-out-of-pair)
  (evil-define-key '(insert) corfu-map (kbd "TAB") 'jump-out-of-pair)

  ;; (define-key prog-mode-map (kbd "<backtab>") 'jump-backward-pair)
  (evil-define-key '(insert) prog-mode-map (kbd "<backtab>") 'jump-backward-pair)
  (evil-define-key '(insert) prog-mode-map (kbd "S-<iso-lefttab>") 'jump-backward-pair)
  (evil-define-key '(insert) corfu-map (kbd "<backtab>") 'jump-backward-pair)
  (evil-define-key '(insert) corfu-map (kbd "S-<iso-lefttab>") 'jump-backward-pair)

  (evil-define-key '(insert) corfu-map (kbd "C-<return>") 'newline-and-indent) ;; <C-return>
  (evil-define-key '(insert) prog-mode-map (kbd "C-<return>") 'newline-and-indent) ;; <C-return>

  ;;     ;; M-g                             corfu-info-location
  ;;     ;; M-h                             corfu-info-documentation
  )

;;; Disable general leader
;;;; General Definer

;; (general-create-definer judy-leader-keys
;;                         :keymaps '(normal insert visual emacs)
;;                         :prefix "SPC"
;;                         :global-prefix "C-SPC")

;;;; Leader-Map
;; (judy-leader-keys
;;   "b" '(:ignore t :which-key "buffer")
;;   "bb" '(switch-to-buffer :which-key "Switch to buffer")
;;   "be" '(eval-buffer :which-key "Evaluate current buffer")
;;   "bd" '(kill-current-buffer :which-key "Kill the current Buffer")
;;   "bk" '(kill-current-buffer :which-key "Kill the current Buffer")
;;   "bm" '(switch-to-messages-buffer :which-key "Switch to Meesage Buffer")
;;   "<" '(switch-to-buffer :which-key "Switch to buffer")

;;   "d" '(:ignore t :which-key "Dired")
;;   "dd" '(dired :which-key "Dired")

;;   "f" '(:ignore t :which-key "files")
;;   "ff" '(find-file :which-key "Find file")

;;   "g" '(:ignore t :which-key "git")
;;   "gs" '(magit-status :which-key "Open magit status for current repo")

;;   "p" '(:ignore t :which-key "programming")
;;   "pm" '(evil-make :which-key "evil-make")
;;   "pe" '(eglot :which-key "Start eglot")
;;   "pr" '(eglot-rename :which-key "rename")
;;   "pd" '(eldoc-doc-buffer :which-key "doc"))

;;;; General Definer

;; (setq general-emit-autoloads nil)

;; (general-define-key
;;  :states '(normal insert motion emacs)
;;  :keymaps 'override
;;  :prefix-map 'tyrant-map
;;  :prefix "SPC"
;;  :non-normal-prefix "M-SPC")

;; (general-create-definer tyrant-def :keymaps 'tyrant-map)
;; (tyrant-def "" nil)

;; (general-create-definer despot-def
;;                         :states '(normal insert motion emacs)
;;                         :keymaps 'override
;;                         :major-modes t
;;                         :prefix "SPC m"
;;                         :non-normal-prefix "M-SPC m")
;; (despot-def "" nil)

;; (general-def universal-argument-map
;;              "SPC u" 'universal-argument-more)

;; (tyrant-def
;;  "SPC"     '("M-x" . execute-extended-command)
;;  "TAB"     '("last buffer" . alternate-buffer)
;;  "!"       '("shell cmd" . shell-command)

;;  "a"       (cons "applications" (make-sparse-keymap))
;;  "ac"      'calc-dispatch
;;  "ap"      'list-processes
;;  "aP"      'proced

;;  "b"       (cons "buffers" (make-sparse-keymap))
;;  "bb" 'consult-buffer
;;  "bB" 'spacemacs/compleseus-switch-to-buffer
;;  "bd" 'spacemacs/kill-this-buffer
;;  "bx" 'kill-buffer-and-window
;;  "bm" 'switch-to-messages-buffer
;;  "ss" 'consult-line
;;  "sS" 'consult-line-symbol
;;  "sg" 'consult-grep
;;  "sd" 'my/compleseus-search-dir
;;  "sD" 'spacemacs/compleseus-search-dir
;;  "sf" 'spacemacs/compleseus-search-auto
;;  "sF" 'my/compleseus-search-auto-hidden
;;  "ff" 'spacemacs/compleseus-find-file
;;  "fs" 'save-buffer
;;  "fD" 'spacemacs/delete-current-buffer-file

;;  "c"       (cons "code" (make-sparse-keymap))
;;  "cb"      'flymake-show-buffer-diagnostics
;;  "cc"      'compile
;;  "cn"      'next-error
;;  "cp"      'previous-error
;;  "cr"      'recompile
;;  "cx"      'kill-compilation
;;  "c="      'indent-region-or-buffer

;;  "f"       (cons "files" (make-sparse-keymap))
;;  "fC"      '("copy-file" . write-file)
;;  "fD"      'delete-current-buffer-file
;;  "fe"      'find-library
;;  "fE"      'sudo-edit
;;  "ff"      'find-file
;;  "fj"      'dired-jump
;;  "fJ"      'dired-jump-other-window
;;  "fo"      'open-file-or-directory-in-external-app
;;  "fr"      'consult-recent-file
;;  "fR"      'rename-current-buffer-file
;;  "fs"      'save-buffer
;;  "fv"      (cons "variables" (make-sparse-keymap))
;;  "fvd"     'add-dir-local-variable
;;  "fvf"     'add-file-local-variable
;;  "fvp"     'add-file-local-variable-prop-line

;;  "F"       (cons "frame" (make-sparse-keymap))
;;  "Fd"      'delete-frame
;;  "FD"      'delete-other-frames
;;  "Fn"      'make-frame
;;  "Fo"      'other-frame

;;  "h"       (cons "help" (make-sparse-keymap))
;;  "ha"      'apropos
;;  "hb"      'describe-bindings
;;  "hc"      'describe-char
;;  "hf"      'describe-function
;;  "hF"      'describe-face
;;  "hi"      'info-emacs-manual
;;  "hI"      'info-display-manual
;;  "hk"      'describe-key
;;  "hK"      'describe-keymap
;;  "hm"      'describe-mode
;;  "hM"      'woman
;;  "hp"      'describe-package
;;  "ht"      'describe-text-properties
;;  "hv"      'describe-variable
;;  "hP"      (cons "profiler" (make-sparse-keymap))
;;  "hPs"     'profiler-start
;;  "hPk"     'profiler-stop
;;  "hPr"     'profiler-report

;;  "j"       (cons "jump" (make-sparse-keymap))
;;  "ji"      'imenu
;;  ;; "jg"      'avy-goto-char-timer

;;  "l"       (cons "layouts" tab-prefix-map)
;;  "ld"      'tab-bar-close-tab
;;  "lD"      'tab-bar-close-other-tabs
;;  "lg"      'tab-bar-change-tab-group
;;  "lm"      'tab-bar-move-tab-to
;;  "lM"      'tab-bar-move-tab-to-group
;;  "ll"      'tab-bar-switch-to-tab
;;  "lR"      'tab-bar-rename-tab
;;  "lt"      'other-tab-prefix
;;  "lu"      'tab-bar-undo-close-tab
;;  "l1"      '("select tab 1..8" . tab-bar-select-tab)
;;  "l2"      'tab-bar-select-tab
;;  "l3"      'tab-bar-select-tab
;;  "l4"      'tab-bar-select-tab
;;  "l5"      'tab-bar-select-tab
;;  "l6"      'tab-bar-select-tab
;;  "l7"      'tab-bar-select-tab
;;  "l8"      'tab-bar-select-tab
;;  "l TAB"   'tab-bar-switch-to-last-tab

;;  "m"       (cons "major mode" (make-sparse-keymap))

;;  "p"       (cons "projects" project-prefix-map)
;;  "pt"      'project-open-in-tab

;;  "q"       (cons "quit" (make-sparse-keymap))
;;  "qd"      'restart-emacs-debug-init
;;  "qr"      'restart-emacs
;;  "qR"      'restart-emacs-without-desktop
;;  "qf"      'delete-frame
;;  "qq"      'save-buffers-kill-terminal
;;  "qQ"      'save-buffers-kill-emacs

;;  "s"       (cons "+search/symbol" (make-sparse-keymap))
;;  "sd"      'spacemacs/compleseus-search-dir
;;  "sg"      'consult-ripgrep

;;  "S"       (cons "spelling" (make-sparse-keymap))
;;  "Sb"      'flyspell-buffer
;;  "Sn"      'flyspell-goto-next-error
;;  "Sr"      'flyspell-region

;;  "k"       (cons "+lisp" (make-sparse-keymap))
;;  "kd"      'delete-pair

;;  "T"       (cons "+Toggles" (make-sparse-keymap))
;;  "Ta"      'auto-fill-mode
;;  "Td"      'toggle-debug-on-error
;;  "Tf"      'display-fill-column-indicator-mode
;;  "Tl"      'toggle-truncate-lines
;;  "Tm"      'flymake-mode
;;  "Tn"      'display-line-numbers-mode
;;  "Ts"      'flyspell-mode
;;  "Tw"      'whitespace-mode
;;  "TW"      'toggle-word-wrap

;;  "u"       '("universal arg" . universal-argument)

;;  "w"       (cons "windows" (make-sparse-keymap))
;;  "w TAB"   'alternate-window
;;  "w+"      'window-layout-toggle
;;  "wb"      'switch-to-minibuffer-window
;;  "wd"      'delete-window
;;  "wD"      'delete-other-windows
;;  "wm"      'toggle-maximize-buffer
;;  "wf"      'follow-mode
;;  "wh"      'evil-window-left
;;  "wH"      'evil-window-move-far-left
;;  "wj"      'evil-window-down
;;  "wJ"      'evil-window-move-very-bottom
;;  "wk"      'evil-window-up
;;  "wK"      'evil-window-move-very-top
;;  "wl"      'evil-window-right
;;  "wL"      'evil-window-move-far-right
;;  "wr"      'rotate-windows-forward
;;  "wR"      'rotate-windows-backward
;;  "ws"      'split-window-vertically
;;  "wS"      'split-window-vertically-and-focus
;;  "wt"      'toggle-current-window-dedication
;;  "wu"      'winner-undo
;;  "wU"      'winner-redo
;;  "wv"      'split-window-horizontally
;;  "wV"      'split-window-horizontally-and-focus)

;; (general-def
;;  [remap comment-dwim] 'comment-or-uncomment
;;  "M-/" 'hippie-expand
;;  "M-j" (defun scroll-other-window-next-line (&optional arg)
;;          (interactive "P")
;;          (scroll-other-window (or arg 1)))
;;  "M-k" (defun scroll-other-window-previous-line (&optional arg)
;;          (interactive "P")
;;          (scroll-other-window (- (or arg 1)))))

;;; flyspell

(with-eval-after-load 'flyspell
  (unbind-key "C-M-i" 'flyspell-mode-map))

(provide 'judy-keys)
;;; judy-keys.el ends here
