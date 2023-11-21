;;; judy-completion.el --- Completion Module -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion using the minad-stack.

;;; Code:

;;; TODO Vertico
(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)
(define-key vertico-map (kbd "C-<backspace>") #'vertico-directory-delete-word)

;;; TODO Corfu
;; Do not auto-complete, ever
;; (customize-set-variable 'corfu-auto nil)

;; Display additional docs next to candidate (formerly corfu-doc)
;; (customize-set-variable 'corfu-popupinfo-delay t)        ;; no delay
;; (customize-set-variable 'corfu-popupinfo-max-height 15)  ;; more docs
;; (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Use Consult to select xref locations with preview
(setq xref-prompt-for-identifier '(not xref-find-definitions
                                       xref-find-definitions-other-window
                                       xref-find-definitions-other-frame
                                       xref-find-references
                                       ;; spacemacs/jump-to-definition
                                       ))
(setq xref-show-xrefs-function #'consult-xref)

;; Configure other variables and modes in the :config section,
;; after lazily loading the package.

;; disable automatic preview by default,
;; selectively enable it for some prompts below.
(setq consult-preview-key '("M-." "C-SPC"))

;; customize preview activation and delay while selecting candiates
(consult-customize
 consult-theme
 :preview-key '("M-." "C-SPC"
                :debounce 0.2 any)

 ;; slightly delayed preview upon candidate selection
 ;; one usually wants quick feedback
 consult-buffer
 consult-ripgrep
 consult-git-grep
 consult-grep
 consult-bookmark
 consult-yank-pop
 :preview-key '("M-." "C-SPC"
                :debounce 0.3 "<up>" "<down>" "C-n" "C-p"
                :debounce 0.6 any))

;; hide magit buffer
(add-to-list 'consult-buffer-filter "magit.*:.*")

(setq consult-line-start-from-top nil)

;; Optionally configure the narrowing key.
;; Both < and C-+ work reasonably well.
(setq consult-narrow-key "<") ;; (kbd "C-+")

;;; kind-icon

(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
(setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 0.9))
(add-hook 'after-load-theme-hook 'kind-icon-reset-cache)

;;; _
(provide 'judy-completion)
;;; judy-completion.el ends here
