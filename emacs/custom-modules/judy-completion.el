;;; judy-completion.el --- Completion Module -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion using the minad-stack.

;;; Code:

;;; TODO Vertico
(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)
(define-key vertico-map (kbd "C-<backspace>") #'vertico-directory-delete-word)

;;; Orderless
(customize-set-variable 'completion-styles '(orderless flex))

;;; TODO Corfu
;; Do not auto-complete, ever
(customize-set-variable 'corfu-auto nil)

;; Display additional docs next to candidate (formerly corfu-doc)
(customize-set-variable 'corfu-popupinfo-delay t)        ;; no delay
(customize-set-variable 'corfu-popupinfo-max-height 15)  ;; more docs
(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)

;;; _
(provide 'judy-completion)
;;; judy-completion.el ends here
