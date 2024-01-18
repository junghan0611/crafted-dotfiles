;;; core-evil.el --- EVIL module -*- lexical-binding: t; -*-

;;; Commentary:

;; core evil
;; replace crated-evil-config.el

;;; Code:

;;; evil

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-respect-visual-line-mode t)

;; C-h is backspace in insert state
(customize-set-variable 'evil-want-C-h-delete t)
(customize-set-variable 'evil-want-C-i-jump t) ; use C-i / C-o  evil-jump-backward/forward
;; "Make a more familiar Vim experience. Take some of the default keybindings
;; for evil mode."
;; C-i / C-o evil-jump-backward/forward
(customize-set-variable 'evil-want-Y-yank-to-eol t)
(customize-set-variable 'evil-want-fine-undo t)

;; C-h is backspace in insert state
(customize-set-variable 'evil-want-C-w-delete t) ; default t
(customize-set-variable 'evil-want-C-u-scroll t) ; default nil

;; FIXME: this correctly causes '*' to match on whole symbols (e.g., on a
;; Clojure file pressing '*' on 'foo.bar' matches the whole thing, instead of
;; just 'foo' or 'bar', BUT, it won't match 'foo.bar' in something like
;; '(foo.bar/baz)', which I don't like.
;; (setq-default evil-symbol-word-search t)

;; (setq evil-move-beyond-eol nil) ; default nil
;; Don't move the block cursor when toggling insert mode
(customize-set-variable 'evil-move-cursor-back nil) ; nil is better - default t
;; Don't put overwritten text in the kill ring
(customize-set-variable 'evil-kill-on-visual-paste nil) ; needed

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
(customize-set-variable 'undo-fu-allow-undo-in-region t)
;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.
(customize-set-variable 'undo-fu-ignore-keyboard-quit t)

;;; Evil Collection or some sparse defaults

(if (locate-library "evil-collection")
    ;; If the user has `evil-collection' installed, initialize it.
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
