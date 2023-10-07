;;; judy-evil.el --- EVIL module -*- lexical-binding: t; -*-

;;; Commentary:

;; Mainly evil-surround

;;; Code:

;;; general.el
(with-eval-after-load 'evil
  (general-evil-setup t)
  )


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

(setq-default evil-want-C-u-scroll t
              ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
              ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
              ;; instead.
              evil-want-C-i-jump nil)

;; (setq evil-move-beyond-eol nil) ; default nil
;; (setq evil-repeat-move-cursor t) ; default t
;; (setq evil-maybe-remove-spaces nil) ; default nil
;; (setq evil-auto-indent nil) ; default t

;; Don't move the block cursor when toggling insert mode
(setq evil-move-cursor-back nil) ; nil is better - default t
;; Don't put overwritten text in the kill ring
(setq evil-kill-on-visual-paste nil) ; needed

;; https://emacs.stackexchange.com/questions/39434/evil-dont-yank-with-only-whitespace-to-register/53536#53536
(with-eval-after-load 'evil-org
  (define-key evil-normal-state-map "x" 'delete-forward-char)
  (define-key evil-normal-state-map "X" 'delete-backward-char)
  (evil-define-key 'normal 'evil-org-mode "x" 'delete-forward-char)
  (evil-define-key 'normal 'evil-org-mode "X" 'delete-backward-char)
  )

;;; _
(provide 'judy-evil)
;;; judy-evil.el ends here
