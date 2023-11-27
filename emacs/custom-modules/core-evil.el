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

;;; _
(provide 'core-evil)
;;; judy-evil.el ends here
