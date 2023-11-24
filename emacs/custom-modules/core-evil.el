;;; core-evil.el --- EVIL module -*- lexical-binding: t; -*-

;;; Commentary:

;; core evil

;;; Code:

;;; evil

(setq evil-want-integration t
      evil-want-keybinding nil
      evil-respect-visual-line-mode t ; spacemacs nil
      evil-undo-system nil
      ;; C-h is backspace in insert state
      evil-want-C-h-delete t
      evil-want-C-w-delete t
      evil-want-C-u-scroll t
      )

  ;; FIXME: this correctly causes '*' to match on whole symbols (e.g., on a
  ;; Clojure file pressing '*' on 'foo.bar' matches the whole thing, instead of
  ;; just 'foo' or 'bar', BUT, it won't match 'foo.bar' in something like
  ;; '(foo.bar/baz)', which I don't like.
  ;; (setq-default evil-symbol-word-search t)

;; (setq evil-move-beyond-eol nil) ; default nil
;; Don't move the block cursor when toggling insert mode
(setq evil-move-cursor-back nil) ; nil is better - default t
;; Don't put overwritten text in the kill ring
(setq evil-kill-on-visual-paste nil) ; needed

;; "Make a more familiar Vim experience. Take some of the default keybindings
;; for evil mode."
;; C-i / C-o evil-jump-backward/forward
(setq evil-want-C-i-jump t
      evil-want-Y-yank-to-eol t
      evil-want-find-undo t)

;; Load Evil and enable it globally
(require 'evil)
(evil-mode 1)

;; Make evil search more like vim
(evil-select-search-module 'evil-search-module 'evil-search)

;; ;; Turn on Evil Nerd Commenter
;; (evilnc-default-hotkeys)

;; Make C-g revert to normal state
(keymap-set evil-insert-state-map "C-g" 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(keymap-global-set "C-M-u" 'universal-argument)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;; Don't create a kill entry on every visual movement.
;; More details: https://emacs.stackexchange.com/a/15054:
(fset 'evil-visual-update-x-selection 'ignore)

;; Prevent evil-motion-state from shadowing previous/next sexp
(require 'evil-maps)
(define-key evil-motion-state-map "L" nil)
(define-key evil-motion-state-map "M" nil)

;;; undo-fu

(require 'undo-fu)
;; C-r 은 isearch-backward 가 기본
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

;; Undo-fu customization options
;; Undoing with a selection will use undo within that region.
(setq undo-fu-allow-undo-in-region t)
;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.
(setq undo-fu-ignore-keyboard-quit t)

;;; Evil Collection or some sparse defaults

(if (locate-library "evil-collection")
    ;; If the user has `evil-collection' installed, initialize it.
    (setq evil-collection-mode-list
          '(2048-game ag alchemist anaconda-mode apropos arc-mode atomic-chrome auto-package-update beginend bluetooth bm bookmark
                      (buff-menu "buff-menu")
                      calc calendar cider cmake-mode color-rg comint company compile consult corfu crdt
                      (custom cus-edit)
                      cus-theme dashboard daemons deadgrep debbugs debug devdocs dictionary diff-hl diff-mode dired dired-sidebar disk-usage distel doc-view docker ebib ebuku edbi edebug ediff eglot elpaca explain-pause-mode eldoc elfeed elisp-mode elisp-refs elisp-slime-nav embark emms emoji epa ert eshell eval-sexp-fu evil-mc eww fanyi finder flycheck flymake forge free-keys geiser ggtags git-timemachine gited gnus go-mode grep guix hackernews helm help helpful hg-histedit hungry-delete ibuffer image image-dired image+ imenu imenu-list
                      (indent "indent")
                      indium info ivy js2-mode leetcode lispy lms log-edit log-view lsp-ui-imenu lua-mode kotlin-mode macrostep man
                      (magit magit-repos magit-submodule)
                      magit-section magit-todos markdown-mode monky mpc mpdel mu4e mu4e-conversation neotree newsticker notmuch nov omnisharp org org-present org-roam osx-dictionary p4 ; outline
                      (package-menu package)
                      pass
                      (pdf pdf-view)
                      popup proced
                      (process-menu simple)
                      prodigy profiler python quickrun racer racket-describe realgud reftex replace restclient rg ripgrep rjsx-mode robe rtags ruby-mode scheme scroll-lock selectrum sh-script shortdoc simple simple-mpc slime sly snake so-long speedbar tab-bar tablist tabulated-list tar-mode telega
                      (term term ansi-term multi-term)
                      tetris thread tide timer-list transmission trashed tuareg typescript-mode vc-annotate vc-dir vc-git vdiff vertico view vlf vterm vundo w3m wdired wgrep which-key woman xref xwidget yaml-mode youtube-dl zmusic
                      (ztree ztree-diff ztree-dir)))
    (setq evil-collection-want-unimpaired-p nil)
    (evil-collection-init)
  ;; otherwise set up some defaults
  (with-eval-after-load 'crafted-completion-config
    (when (featurep 'vertico) ; only if `vertico' is actually loaded.
      (keymap-set vertico-map "C-j" #'vertico-next)
      (keymap-set vertico-map "C-k" #'vertico-previous)
      (keymap-set vertico-map "M-h" #'vertico-directory-up))))


;;; _
(provide 'core-evil)
;;; judy-evil.el ends here
