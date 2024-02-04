;;; judy-dev.el --- Emacs as IDE -*- lexical-binding: t; -*-
;;; Commentary:

;; Set up dev and project management

;;; Code:

;;; eldoc / package-lint-flymake

;; Global defaults
(require 'eldoc)

(when (locate-library "package-lint-flymake")
    (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup))

;;; Parens Helpers

;; Use puni-mode only for certain major modes.
(require 'puni)

(let ((map puni-mode-map))
  ;; ("M-<backspace>" . puni-splice)
  (define-key map (kbd "M-<delete>") #'puni-splice) ; sp-unwrap-sexp
  (define-key map (kbd "C-<right>")  #'puni-slurp-forward)
  (define-key map (kbd "C-<left>") #'puni-barf-forward)

  (define-key map (kbd "C-M-<left>") 'puni-slurp-backward)
  (define-key map (kbd "C-M-<right>") 'puni-barf-backward)

  ;; ("C-M-<delete>" . puni-splice-killing-forward)
  ;; ("C-M-<backspace>" . puni-splice-killing-backward)

  (define-key map (kbd "C-M-a") 'beginning-of-defun) ; default
  (define-key map (kbd "C-M-e") 'end-of-defun)

  ;; ("M-]" . forward-sexp) ; default
  ;; ("M-[" . backward-sexp)

  ;; ("C-M-f" . puni-forward-sexp)
  ;; ("C-M-b" . puni-backward-sexp)

  ;; ("C-M-p" . puni-beginning-of-sexp)
  ;; ("C-M-n" . puni-end-of-sexp)

  ;; ("C-M-t" . transpose-sexp)
  ;; ("C-M-?" . puni-convolute)

  ;; ("C-M-k" . kill-sexp)
  ;; ("C-M-K"   . backward-kill-sexp)

  ;; ("M-)" . puni-syntactic-forward-punct)
  ;; ("M-(" . puni-syntactic-backward-punct)

  ;; ("C-c DEL" . puni-force-delete)
  ;; ("C-M-z" . puni-squeeze) ; unwrap

  ;; ("C-c {" . puni-wrap-curly)
  ;; ("C-c (" . puni-wrap-round)
  ;; ("C-c [" . puni-wrap-square)
  )

(dolist (hook '(sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook))
  (add-hook hook #'puni-mode))

(dolist (hook '(emacs-lisp-mode-hook clojure-mode-hook))
  (add-hook hook #'puni-mode))

(add-hook 'puni-mode-hook 'electric-pair-mode)

;;; diff-hl

(require 'diff-hl)
;; Highlight changes since last commit
(customize-set-variable 'diff-hl-side 'right)
(add-hook 'prog-mode-hook #'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook #'turn-on-diff-hl-mode)

;;; Autoscroll compile buffer

(customize-set-variable 'compilation-scroll-output t)

;;; clojure

(with-eval-after-load 'clojure
  (add-to-list 'auto-mode-alist '("\\.endl$" . clojure-mode))
  (add-to-list 'magic-mode-alist '("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode))

  (add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))

  (setq clojure-indent-style 'align-arguments)
  ;; Vertically align s-expressions
  ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
  (setq clojure-align-forms-automatically t)
  )

(with-eval-after-load 'cider
  (setq cider-repl-result-prefix ";; => "
        cider-eval-result-prefix ""
        cider-connection-message-fn nil ; cute, but no!
        cider-repl-prompt-function #'my/cider-repl-prompt
        ;; cider-use-overlays nil ; echo area is fine
        )

  (defun my/cider-repl-prompt (namespace)
    "Return a prompt string that mentions NAMESPACE."
    (format "%s🦄 " (cider-abbreviate-ns namespace)))

  (setq
   cider-repl-display-help-banner nil        ;; enable help banner
   ;; cider-print-fn 'puget                   ;; pretty printing with sorted keys / set values
   clojure-align-forms-automatically t
   ;; clojure-toplevel-inside-comment-form t
   ;; cider-result-overlay-position 'at-point   ; results shown right after expression
   ;; cider-overlays-use-font-lock t
   cider-repl-buffer-size-limit 100          ; limit lines shown in REPL buffer
   nrepl-use-ssh-fallback-for-remote-hosts t ; connect via ssh to remote hosts
   cider-preferred-build-tool 'clojure-cli
   )
  )

;; Note: Ensure CIDER and lsp-mode play well together, as we use both.
;; - LSP for more static-analysis-y services (completions, lookups, errors etc.),
;; - CIDER for "live" runtime services (enhanced REPL, interactive debugger etc.).

;; /home/junghan/sync/man/dotsamples/vanilla/evalapply-dotfiles-clojure/init.el
;; Use clojure-lsp for eldoc and completions
;; layer 에서 eldoc 을 빼주면 된다. lsp 로만 동작하게 된다.
;; h/t cider docs and ericdallo/dotfiles/.config/doom/config.el
;; (remove-hook 'eldoc-documentation-functions #'cider-eldoc)
;; (remove-hook 'completion-at-point-functions #'cider-complete-at-point)

;; settings h/t suvratapte/dot-emacs-dot-d
;; (setq
;;  ;; cider-prompt-for-symbol nil
;;  ;; play nice with lsp-mode
;;  ;; h/t ericdallo/dotfiles/.config/doom/config.el
;;  cider-font-lock-dynamically nil ; use lsp semantic tokens
;;  cider-eldoc-display-for-symbol-at-point nil ; use lsp
;;  cider-prompt-for-symbol nil ; use lsp
;;  cider-use-xref nil ; use lsp
;;  ;; Maybe customize variables for cider-jack-in
;;  ;; https://docs.cider.mx/cider/basics/up_and_running.html
;;  )

;;; C/C++
;; (defun clang-format-buffer-with-config ()
;;   "Format current buffer using projects' .clang-format file."
;;   (interactive)
;;   (unless (project-current ".")
;;       (message "Unable to format buffer: No project."))
;;   (when (file-exists-p (expand-file-name ".clang-format" (project-root (project-current))))
;;     (clang-format-buffer)))

;; (defun clang-format-buffer-on-save ()
;;   "Add clang-format-buffer-with-config to before-save-hook."
;;   (add-hook 'before-save #'clang-format-buffer-with-config nil t))

;; (add-hook 'c-mode-hook #'clang-format-buffer-on-save)
;; (add-hook 'c++-mode-hook #'clang-format-buffer-on-save)

;;; Rust
;; (customize-set-variable 'rust-format-on-save t)

;; ;; Allow project.el to find Rust projects by Cargo.toml file
;; (defun judy--project-find-rust-project (dir)
;;   "Find rust project by Cargo.toml instead of VC.
;; Useful when multiple rust projects reside in the same VC repo."
;;   (let ((override (locate-dominating-file dir "Cargo.toml")))
;;     (if override
;;         (list 'vc 'Git override)
;;       nil)))

;; (add-hook 'project-find-functions #'judy--project-find-rust-project)

;;; Lisp
(require 'outline)
(define-key outline-minor-mode-map (kbd "<backtab>") #'outline-cycle)

;; Add outline-minor-mode to lisp-modes
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'lisp-mode-hook #'outline-minor-mode)
(add-hook 'clojure-mode-hook #'outline-minor-mode)

;;; outli

;; (outli :location (recipe :fetcher github :repo "jdtsmith/outli"))
(require 'outli)

(customize-set-variable 'outli-blend nil)

;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
(add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
(add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
(add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
(add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
(add-to-list 'outli-heading-config '(python-mode "##" ?# t))
(add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
(add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
;; (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
(add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
(add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
(add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
(add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))
(add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
(add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))
;; check - outline-mode-map
;; :bind (:map outli-mode-map ; convenience key to get back to containing heading
;;             ("C-c o" . (lambda () (interactive) (outline-back-to-heading))))
(add-hook 'prog-mode-hook 'outli-mode) ; not markdown-mode!

(with-eval-after-load 'outli

  ;; evil normal keybinding is perfer
  ;; (evil-define-key '(normal visual) outli-mode-map (kbd "S-<tab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
  ;; (evil-define-key '(normal visual) outli-mode-map (kbd "S-TAB") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
  ;; (evil-define-key '(normal visual) outli-mode-map (kbd "<backtab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
  ;; (evil-define-key '(normal visual) outli-mode-map (kbd "S-<iso-lefttab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))

  (evil-define-key '(normal visual) outli-mode-map (kbd "S-<tab>") 'recenter-top-bottom)
  (evil-define-key '(normal visual) outli-mode-map (kbd "S-TAB") 'recenter-top-bottom)
  (evil-define-key '(normal visual) outli-mode-map (kbd "<backtab>") 'recenter-top-bottom)
  (evil-define-key '(normal visual) outli-mode-map (kbd "S-<iso-lefttab>") 'recenter-top-bottom)

  ;; 'TAB' for terminal emacs
  (evil-define-key '(normal visual) outli-mode-map (kbd "<tab>") `(menu-item "" outline-cycle :filter outli--on-heading))
  (evil-define-key '(normal visual) outli-mode-map (kbd "TAB") `(menu-item "" outline-cycle :filter outli--on-heading))

  (evil-define-key '(normal visual) prog-mode-map (kbd "<tab>") 'indent-for-tab-command)
  (evil-define-key '(normal visual) prog-mode-map (kbd "TAB") 'indent-for-tab-command)

  (evil-define-key '(normal) outli-mode-map (kbd "C-c 1") (lambda () (interactive) (outline--show-headings-up-to-level 1)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 2") (lambda () (interactive) (outline--show-headings-up-to-level 2)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 3") (lambda () (interactive) (outline--show-headings-up-to-level 3)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 4") (lambda () (interactive) (outline--show-headings-up-to-level 4)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 5") (lambda () (interactive) (outline--show-headings-up-to-level 5)))

  (evil-define-key '(normal) outli-mode-map (kbd "C-M-<tab>") 'outline-cycle-buffer)

  ;; (define-key outli-mode-map (kbd "C-M-<iso-lefttab>")
  ;;             (lambda () (interactive) (outline-cycle-buffer)))

  (evil-define-key '(normal insert) outli-mode-map (kbd "C-n") 'outline-next-visible-heading) ; this works
  (evil-define-key '(normal insert) outli-mode-map (kbd "C-p") 'outline-previous-visible-heading)

  (define-key prog-mode-map (kbd "C-c H") 'outline-insert-heading)
  (define-key prog-mode-map (kbd "C-c o") 'consult-outline)
  )

;;;; Common Lisp
;; (customize-set-variable 'inferior-lisp-program (if (executable-find "ros")
;;                                                    "ros run"
;;                                                  "sbcl"))

;;; Web-mode
(with-eval-after-load 'web-mode
  (customize-set-variable 'web-mode-markup-indent-offset 4)
  (customize-set-variable 'web-mode-css-indent-offset 4)
  (customize-set-variable 'web-mode-code-indent-offset 4)
  (customize-set-variable 'web-mode-indent-style 4))

;;; Exercism

(require 'exercism)
(global-set-key (kbd "M-c e") 'exercism)
(global-set-key (kbd "M-g e") 'exercism)

;; (when (require 'exercism nil :noerror)
;;   (customize-set-variable 'exercism-display-tests-after-run t)
;;   (keymap-global-set "M-g 0" 'exercism)
;;   )

;; (require 'awk-ts-mode)
(require 'bats-mode)

;;; apheleia

(require 'apheleia)
(add-hook 'markdown-mode-hook 'apheleia-mode)
(add-hook 'yaml-mode-hook 'apheleia-mode)

;;;###autoload
(defun my/format-buffer ()
  "Format a buffer."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (indent-region (point-min) (point-max)))
   ((eq major-mode 'ledger-mode)
    (ledger-mode-clean-buffer))
   (t (call-interactively 'apheleia-format-buffer))))

(global-set-key (kbd "M-g =") 'my/format-buffer)

;;; aggressive-indent for emacs-lisp-mode

;; aggressive-indent-mode for all lisp modes
(when (locate-library "aggressive-indent")
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(add-hook 'emacs-lisp-mode-hook
    (lambda ()
        (setq-local comment-column 0)
        (electric-indent-mode -1)
        (define-key emacs-lisp-mode-map (kbd "M-[") 'backward-sexp)
        (define-key emacs-lisp-mode-map (kbd "M-]") 'forward-sexp)))

;;; eglot (Language Server)

;; Auto-shutdown eglot (when all associated buffers are killed)
(customize-set-variable 'eglot-autoshutdown t)

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c d") 'eldoc) ;; use eldoc-toggle
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename))

;;; eldoc

;; recommand echo area as simple as possible
(customize-set-variable 'eldoc-echo-area-use-multiline-p nil) ; important

;; eldoc-echo-area-prefer-doc-buffer t ; default nil - aloway show echo-area
;; ;; eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer)
;; ;; eldoc-documentation-strategy 'eldoc-documentation-compose)

(defun eldoc-toggle ()
  "Toggle eldoc's documentation buffer."
  (interactive)
  (let ((buffer (eldoc-doc-buffer)))
    (if-let (w (and buffer (get-buffer-window buffer)))
        (delete-window w)
      (eldoc-doc-buffer t))))
(global-set-key (kbd "C-M-'") 'eldoc-toggle)

;;; python ide

;; use editorconfig
(customize-set-variable 'python-indent-offset 4)

;; pipenv 로 커버

(add-hook 'python-ts-mode-hook
    (lambda ()
        (define-key python-ts-mode-map (kbd "<f5>") 'recompile)
        (define-key python-ts-mode-map (kbd "<f6>") 'eglot-format)))

;;; combobulate
;; https://github.com/mickeynp/combobulate

;;  M-x customize-group RET combobulate RET
(require 'combobulate)
(require 'html-ts-mode)

(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "M-g f") 'consult-flymake)

;; You can customize Combobulate's key prefix here.
;; Note that you may have to restart Emacs for this to take effect!
(customize-set-variable 'combobulate-key-prefix "C-c o")

(global-set-key (kbd "C-c 0") 'combobulate)

;; Optional, but recommended. Tree-sitter enabled major modes are
;; distinct from their ordinary counterparts.
;;
;; You can remap major modes with `major-mode-remap-alist'. Note
;; that this does *not* extend to hooks! Make sure you migrate them
(dolist (mapping '((python-mode . python-ts-mode)
                   (css-mode . css-ts-mode)
                   (typescript-mode . tsx-ts-mode)
                   (json-mode . json-ts-mode)
                   (js-mode . js-ts-mode)
                   (css-mode . css-ts-mode)
                   (html-mode . html-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(remove-hook 'prog-mode-hook #'combobulate-mode)

;; You can manually enable Combobulate with `M-x ;; combobulate-mode'.
(dolist (hook '(python-ts-mode-hook js-ts-mode-hook css-ts-mode-hook yaml-ts-mode-hook
                   html-ts-mode-hook json-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
    (add-hook hook #'combobulate-mode))

;;; indent-bars for python-ts-mode

;; (require 'indent-bars)

;; (setq indent-bars-treesit-support t
;;     indent-bars-no-descend-string t
;;     indent-bars-treesit-ignore-blank-lines-types '("module")
;;     indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
;;                                    list list_comprehension
;;                                    dictionary dictionary_comprehension
;;                                    parenthesized_expression subscript)))
;; (add-hook 'python-ts-mode-hook 'indent-bars-mode)

;; ;; simple
;; ;; (setq
;; ;;     indent-bars-pattern "."
;; ;;     indent-bars-width-frac 0.5
;; ;;     indent-bars-pad-frac 0.25
;; ;;     indent-bars-color-by-depth nil
;; ;;     indent-bars-highlight-current-depth '(:face default :blend 0.4))
;; ;; terminal
;; (setq
;;     indent-bars-color '(highlight :face-bg t :blend 0.75)
;;     indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
;;     indent-bars-unspecified-fg-color "white"
;;     indent-bars-unspecified-bg-color "black")

;;; _
(provide 'judy-dev)
;;; judy-dev.el ends here
