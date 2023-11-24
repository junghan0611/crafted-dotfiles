;;; judy-evil.el --- EVIL module -*- lexical-binding: t; -*-

;;; Commentary:

;; Mainly evil-surround

;;; Code:

(require 'undo-fu)
;; C-r 은 isearch-backward 가 기본
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

;; Undo-fu customization options
;; Undoing with a selection will use undo within that region.
(setq undo-fu-allow-undo-in-region t)
;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.
(setq undo-fu-ignore-keyboard-quit t)
;; By default while in insert all changes are one big blob. Be more granular
(setq evil-want-fine-undo t)

;;; more motions
;; Use Ctrl + vim-up/down to move between headings in org-mode
;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

;; from DW
(evil-define-key '(normal insert visual) org-mode-map (kbd "C-n") 'org-next-visible-heading)
(evil-define-key '(normal insert visual) org-mode-map (kbd "C-p") 'org-previous-visible-heading)

;; Rebind universal argument
(global-set-key (kbd "C-M-u") 'universal-argument)

;; Add binding for ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set default mode for system text buffers to normal mode
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

;; Some defaults
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-want-abbrev-expand-on-insert-exit nil)

;;; evil-surround
(defun my/evil-org-additional-surround-pairs ()
  "Additional surround pairs for evil-surround in org-mode."
  (push '(?| . ("| " . " |")) evil-surround-pairs-alist))

(add-hook 'org-mode-hook #'my/evil-org-additional-surround-pairs)
(global-evil-surround-mode t)

;;; evil

(setq-default evil-want-C-u-scroll t)

;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
;; overlapped in terminal mode. The GUI specific `<C-i>' is used
;; instead.

(setq-default evil-want-C-i-jump t) ; use C-i / C-o  evil-jump-backward/forward

(setq evil-want-C-h-delete nil)
(setq evil-want-C-w-delete nil)

;; Don't move the block cursor when toggling insert mode
(setq evil-move-cursor-back nil) ; nil is better - default t
;; Don't put overwritten text in the kill ring
(setq evil-kill-on-visual-paste nil) ; needed

(setq evil-move-beyond-eol nil) ; default nil

;; Don't create a kill entry on every visual movement.
;; More details: https://emacs.stackexchange.com/a/15054:
(fset 'evil-visual-update-x-selection 'ignore)

;; https://emacs.stackexchange.com/questions/39434/evil-dont-yank-with-only-whitespace-to-register/53536#53536
(with-eval-after-load 'evil-org
  (define-key evil-normal-state-map "x" 'delete-forward-char)
  (define-key evil-normal-state-map "X" 'delete-backward-char)
  (evil-define-key 'normal 'evil-org-mode "x" 'delete-forward-char)
  (evil-define-key 'normal 'evil-org-mode "X" 'delete-backward-char)
  )

;; Prevent evil-motion-state from shadowing previous/next sexp
(require 'evil-maps)
(define-key evil-motion-state-map "L" nil)
(define-key evil-motion-state-map "M" nil)

;;;; winner

(require 'winner)
(setq winner-boring-buffers-regexp "\\*.*\\*")
(winner-mode +1)
;; C-c <left>   winner-undo
;; C-c <right>  winner-redo
(define-key evil-window-map "u" 'winner-undo)
(define-key evil-window-map "U" 'winner-redo)
;; (define-key winner-mode-map (kbd "M-s-[") #'winner-undo)
;; (define-key winner-mode-map (kbd "M-s-]") #'winner-redo)

;;; hungry-delete

(require 'hungry-delete)

(nconc hungry-delete-except-modes '(term-mode vterm-mode))
(setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace

;; layers/+emacs/better-defaults/keybindings.el
(defun spacemacs/backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      ;; call interactively so kill-region handles rectangular selection
      ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

;; 익숙한 키 바인딩이라. 그냥 두자.
(global-set-key (kbd "M-<backspace>") 'spacemacs/backward-kill-word-or-region)

;; [x] 모드와 상관 없이 backsapce 는 delete-backward-char 안된다. : conflict
;; 모드와 상관 없이 Delete 키는 delete-forward-char : default

;; evil 바인딩으로 해야 먹힌다.

(define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
;; (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)
(define-key hungry-delete-mode-map (kbd "S-<backspace>") 'hungry-delete-backward)
(define-key hungry-delete-mode-map (kbd "S-<delete>") 'hungry-delete-forward)
(define-key hungry-delete-mode-map (kbd "S-DEL") 'hungry-delete-forward)

;; 기본 스타일 바인딩을 사용하자.
(global-set-key (kbd "S-<backspace>") 'hungry-delete-backward) ; default bindings
(global-set-key (kbd "S-<delete>") 'hungry-delete-forward)
(global-set-key (kbd "S-DEL") 'hungry-delete-forward)

(global-hungry-delete-mode t)

;;; evil

(require 'evil-textobj-tree-sitter)
;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;; You can also bind multiple items and we will match the first one we can find
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))


;;; _
(provide 'judy-evil)
;;; judy-evil.el ends here
