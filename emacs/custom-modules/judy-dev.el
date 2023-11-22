;;; judy-dev.el --- Emacs as IDE -*- lexical-binding: t; -*-
;;; Commentary:

;; Set up dev and project management

;;; Code:

;;; TODO Spell checking

;; (add-hook 'after-init #'global-flycheck-mode)

;; (customize-set-variable 'flymake-aspell-aspell-options
;;                         '("--sug-mode=normal" "--lang=de"))
;; (add-hook 'text-mode-hook #'flymake-aspell-setup)

;;; eglot (Language Server)
;; Auto-shutdown eglot (when all associated buffers are killed)
(customize-set-variable 'eglot-autoshutdown t)

;;; Parens Helpers
;; (require 'smartparens-config)
;; (smartparens-global-mode)

;; Use puni-mode only for certain major modes.
(require 'puni)
(dolist (hook '(prog-mode-hook sgml-mode-hook nxml-mode-hook tex-mode-hook eval-expression-minibuffer-setup-hook))
  (add-hook hook #'puni-mode))

;;; diff-hl
;; Highlight changes since last commit
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
(setq outli-blend nil)
(add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
(add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))
;; check - outline-mode-map
;; :bind (:map outli-mode-map ; convenience key to get back to containing heading
;;             ("C-c o" . (lambda () (interactive) (outline-back-to-heading))))
(add-hook 'prog-mode-hook 'outli-mode) ; not markdown-mode!

;;;; Common Lisp
;; (customize-set-variable 'inferior-lisp-program (if (executable-find "ros")
;;                                                    "ros run"
;;                                                  "sbcl"))

;;; Web-mode
(with-eval-after-load 'web-mode
  (customize-set-variable 'web-mode-markup-indent-offset 2)
  (customize-set-variable 'web-mode-css-indent-offset 2)
  (customize-set-variable 'web-mode-code-indent-offset 2)
  (customize-set-variable 'web-mode-indent-style 2))

;;; Exercism

;;(global-set-key (kbd "M-c e") 'exercism)

;; (when (require 'exercism nil :noerror)
;;   (customize-set-variable 'exercism-display-tests-after-run t)
;;   (keymap-global-set "M-g 0" 'exercism)
;;   )

;; (require 'exercism)
(require 'awk-ts-mode)
(require 'bats-mode)
(bats-mode)

;; (require 'asdf)
;; (asdf-enable)

;;; _
(provide 'judy-dev)
;;; judy-dev.el ends here
