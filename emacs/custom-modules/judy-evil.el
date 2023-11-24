;;; judy-evil.el --- EVIL module -*- lexical-binding: t; -*-

;;; Commentary:

;; Mainly evil-surround

;;; Code:

;;; Which Key

(require 'which-key)

(setq which-key-idle-delay 0.4
      which-key-min-display-lines 6
      which-key-idle-secondary-delay 0.01
      which-key-max-description-length 40
      which-key-sort-order 'which-key-key-order-alpha
      which-key-allow-evil-operators t
      )

(which-key-mode +1)

;;; more motions

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

;; Replace Emacs Tabs key bindings with Workspace key bindings
(with-eval-after-load 'evil-maps
  ;; 되는 것인가?
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-motion-state-map "gc" 'evilnc-comment-operator)
  ;; 편집 창 포커스 이동을 간단하게
  ;; (define-key evil-normal-state-map (kbd "<SPC> <right> ") 'evil-window-right)
  ;; (define-key evil-normal-state-map (kbd "<SPC> <left> ") 'evil-window-left)
  ;; (define-key evil-normal-state-map (kbd "<SPC> <up> ") 'evil-window-up)
  ;; (define-key evil-normal-state-map (kbd "<SPC> <down> ") 'evil-window-down)

  ;; replace "." search with consul-line in Evil normal state
  ;; use default "/" evil search
  ;; (evil-global-set-key 'normal "." 'consult-line)

  (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)
  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)
  ;; =C-w= 'insert 'evil-delete-backward-word
  ;; =C-w= 'visual 'evil-window-map
  )

;;; evil-surround
(defun my/evil-org-additional-surround-pairs ()
  "Additional surround pairs for evil-surround in org-mode."
  (push '(?| . ("| " . " |")) evil-surround-pairs-alist))

(add-hook 'org-mode-hook #'my/evil-org-additional-surround-pairs)
(global-evil-surround-mode t)


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

;;; evil-textobj-tree-sitter

(require 'evil-textobj-tree-sitter)
;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;; You can also bind multiple items and we will match the first one we can find
(define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

;;; hangul input method

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

;;; evil-escape

(require 'evil-escape)

;; evil-escape - switch between insert and normal state
;; fd 는 ㄹㅇ일 때 적용이 안되니 ,.을 입력 시 escape 하도록 바꿈.
;; unordered 로 해보니 minor-mode 를 열기도 해서 아예 논란이 없도록 바꿈.
(setq-default evil-escape-key-sequence ",.")
(setq-default evil-escape-unordered-key-sequence nil)
(setq-default evil-escape-delay 1.0) ;; 0.5, default 0.1
;; (setq-default evil-escape-inhibit-functions nil)

(evil-escape-mode)
;; (add-to-list 'evil-escape-excluded-major-modes 'code-review-mode)

;;; _
(provide 'judy-evil)
;;; judy-evil.el ends here
