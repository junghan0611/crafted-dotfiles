;;; judy-completion.el --- Completion Module -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion using the minad-stack.

;;; Code:

;;; TODO Consult Vertico
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

(unless *is-termux*
  (setq vertico-count 20)
  (setq vertico-resize 'grow-only))
(when *is-termux*
  (setq vertico-resize nil)
  (setq vertico-count 7))

;; customize preview activation and delay while selecting candiates
(consult-customize
 consult-theme
 :preview-key '("M-." "C-SPC"
                :debounce 3.0 any)

 ;; slightly delayed preview upon candidate selection
 ;; one usually wants quick feedback
 consult-buffer
 consult-ripgrep
 consult-git-grep
 consult-grep
 consult-bookmark
 consult-yank-pop

 consult-line ; :prompt "Consult-line: "
 consult-recent-file
 consult-xref
 consult-org-heading
 consult-outline ; 2023-05-23
 spacemacs/consult-line
 spacemacs/compleseus-switch-to-buffer
 spacemacs/compleseus-search-dir
 spacemacs/compleseus-search-auto
 spacemacs/compleseus-find-file ; 2023-05-14 추가
 spacemacs/embark-preview ; 2023-05-23
 spacemacs/compleseus-search-default
 my/compleseus-search-dir

 :preview-key '("M-." "C-SPC"
                :debounce 0.3 "<up>" "<down>" "C-n" "C-p"
                ;; :debounce 0.6 any
                ))

;; hide magit buffer
(add-to-list 'consult-buffer-filter "magit.*:.*")

(setq consult-line-start-from-top nil)

;; Optionally configure the narrowing key.
;; Both < and C-+ work reasonably well.
(setq consult-narrow-key "<") ;; (kbd "C-+")

(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)
(define-key vertico-map (kbd "C-l") #'vertico-insert)
(define-key vertico-map (kbd "C-S-j") #'vertico-next-group)
(define-key vertico-map (kbd "C-S-k") #'vertico-previous-group)
(define-key vertico-map (kbd "C-n") #'spacemacs/next-candidate-preview)
(define-key vertico-map (kbd "C-p") #'spacemacs/previous-candidate-preview)

(define-key vertico-map (kbd "M-RET") #'vertico-exit-input)
(define-key vertico-map (kbd "C-SPC") #'spacemacs/embark-preview)
(define-key vertico-map (kbd "C-r") #'consult-history)

(define-key vertico-map (kbd "C-<backspace>") #'vertico-directory-delete-word)

;;; TODO Corfu/Cape

(require 'corfu)
(setq corfu-bar-width 0.5
      corfu-auto-delay 0.3
      corfu-on-exact-match nil
      corfu-preselect nil
      corfu-min-width 35
      corfu-max-width 80
      )

(define-key corfu-map (kbd "M-.") #'corfu-move-to-minibuffer)

;;; kind-icon

(require 'kind-icon)
(setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
(setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 0.9))
(add-hook 'after-load-theme-hook 'kind-icon-reset-cache)

;;; tempel

(require 'tempel)
;; (setq tempel-trigger-prefix "<") ; conflits with evil-shift
(setq tempel-path (expand-file-name "tempel-templates.eld" user-emacs-directory))
;; Use concrete keys because of org mode
;; "M-RET" #'tempel-done
;; "M-{" #'tempel-previous
;; "M-}" #'tempel-next
;; "M-<up>" #'tempel-previous
;; "M-<down>" #'tempel-next

;; 2023-10-19 disable my custom
(define-key tempel-map (kbd "RET") #'tempel-done)
(define-key tempel-map (kbd "M-n") #'tempel-next)
(define-key tempel-map (kbd "M-p") #'tempel-previous)

(global-set-key (kbd "M-+") 'tempel-complete)
(global-set-key (kbd "M-*") 'tempel-insert)

;;; embark

;; (("M-o" . embark-act)         ;; pick some comfortable binding
;;  ("C-;" . embark-dwim)        ;; good alternative: M-.
;;  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

(require 'embark)

;; this gets you the available-key preview minibuffer popup
(setq prefix-help-command #'embark-prefix-help-command
      ;; don't use C-h for paging, instead `describe-prefix-bindings`.
      which-key-use-C-h-commands nil)
;; same key binding as ivy-occur
(define-key minibuffer-local-map (kbd "C-c C-o") #'embark-export)
(define-key minibuffer-local-map (kbd "C-c C-l") #'embark-collect)
;; mimic action key bindings from helm
(define-key minibuffer-local-map (kbd "C-z") #'spacemacs/embark-action-completing-read)
(define-key minibuffer-local-map (kbd "C-c C-e") #'spacemacs/consult-edit)
;; which keys nice display
(which-key-add-keymap-based-replacements minibuffer-local-map "C-c C-o" "Embark export")
(which-key-add-keymap-based-replacements minibuffer-local-map "C-c C-l" "Embark collect")
(which-key-add-keymap-based-replacements minibuffer-local-map "C-c C-e" "Edit buffer")
(which-key-add-keymap-based-replacements minibuffer-local-map "C-z" "Embark actions...")
(define-key embark-file-map "s" 'spacemacs/compleseus-search-from)

;; which key integration setup
;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
(setq embark-indicators
      '(spacemacs/embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))
(advice-add #'embark-completing-read-prompter
            :around #'spacemacs/embark-hide-which-key-indicator)

(global-set-key (kbd "M-o") 'embark-act)
(global-set-key (kbd "C-;") 'embark-dwim)
(global-set-key (kbd "C-h B") 'embark-bindings)

;;; display-buffer-alist

;; /prot-dotfiles/emacs/.emacs.d/prot-emacs-modules/prot-emacs-window.el:55
(add-to-list 'display-buffer-alist
             `("\\*\\(Output\\|Register Preview\\).*"
               (display-buffer-reuse-mode-window display-buffer-at-bottom)))
(add-to-list 'display-buffer-alist
             `("\\*\\(Calendar\\|Bookmark Annotation\\|Buffer List\\).*"
               (display-buffer-reuse-mode-window display-buffer-below-selected)
               (window-height . fit-window-to-buffer)))

(add-to-list 'display-buffer-alist
             ;; bottom side window
             `("\\*Org Select\\*" ; the `org-capture' key selection
               (display-buffer-in-side-window)
               (dedicated . t)
               (side . bottom)
               (slot . 0)
               (window-parameters . ((mode-line-format . none)))))
(add-to-list 'display-buffer-alist
             `("\\*Embark Actions\\*"
               (display-buffer-reuse-mode-window display-buffer-at-bottom)
               (window-height . fit-window-to-buffer)
               (window-parameters . ((no-other-window . t)
                                     (mode-line-format . none)))))

;;; _
(provide 'judy-completion)
;;; judy-completion.el ends here
