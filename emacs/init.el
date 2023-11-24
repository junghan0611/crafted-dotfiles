;;; init.el --- Configuring Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Init loading up Crafted Emacs.

;;; Code:

;;; Custom file

(customize-set-variable 'crafted-startup-inhibit-splash t)

(setq custom-file
      (expand-file-name ".cache/custom-vars.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil :nomessage))

;;; pinned-melpa-packages

(defvar my/pinned-melpa-packages
  '(
    corfu
    kind-icon
    evil-org
    tempel
    promise
    nerd-icons
    nerd-icons-dired
    nerd-icons-completion
    exercism
    treesit-auto
    magit
    evil-textobj-tree-sitter
    dash-functional
    )
  )

(customize-set-variable 'package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "melpa"))
           my/pinned-melpa-packages)))

;;; Bootstrap Crafted Emacs
(load (expand-file-name "modules/crafted-init-config.el" crafted-emacs-home))

(setq treesit-extra-load-path `(,(concat user-emacs-directory "tree-sitter/")))

;;; Configure packages to install

(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-ide-packages)
(require 'crafted-lisp-packages)
(require 'crafted-org-packages)
(require 'crafted-ui-packages)
(require 'crafted-writing-packages)

;;;; Additional packages for custom modules

;; judy-keys
;; (add-to-list 'package-selected-packages 'general)
;; (add-to-list 'package-selected-packages 'combobulate)
(add-to-list 'package-selected-packages 'which-key)
(add-to-list 'package-selected-packages 'pcre2el)
(add-to-list 'package-selected-packages 'doom-modeline)
(add-to-list 'package-selected-packages 'winum)
(add-to-list 'package-selected-packages 'kind-icon)
(add-to-list 'package-selected-packages 'avy)
(add-to-list 'package-selected-packages 'string-edit-at-point)
(add-to-list 'package-selected-packages 'expand-region)
(add-to-list 'package-selected-packages 'goto-last-change)
(add-to-list 'package-selected-packages 'nerd-icons-dired)
(add-to-list 'package-selected-packages 'nerd-icons-completion)
(add-to-list 'package-selected-packages 'imenu-list)
(add-to-list 'package-selected-packages 'undo-fu)
(add-to-list 'package-selected-packages 'tempel)
(add-to-list 'package-selected-packages 'neotree)
(add-to-list 'package-selected-packages 'revert-buffer-all)

;; (add-to-list 'package-selected-packages 'citar)
;; (add-to-list 'package-selected-packages 'citar-denote)

(add-to-list 'package-selected-packages 'evil-escape)
(add-to-list 'package-selected-packages 'popper)
;; (add-to-list 'package-selected-packages 'popwin)

;; (add-to-list 'package-selected-packages 'doom-themes)
;; (add-to-list 'package-selected-packages 'ct)
;; (add-to-list 'package-selected-packages 'auto-dim-other-buffers)
;; (add-to-list 'package-selected-packages 'rainbow-mode)

(add-to-list 'package-selected-packages 'hydra)
(add-to-list 'package-selected-packages 'major-mode-hydra) ; contains pretty-hydra

;; judy-theme
(unless (member 'modus-vivendi (custom-available-themes))
  (add-to-list 'package-selected-packages 'modus-themes))

(add-to-list 'package-selected-packages 'ef-themes)
;; (add-to-list 'package-selected-packages 'fontaine)
(add-to-list 'package-selected-packages 'keycast)
(add-to-list 'package-selected-packages 'apheleia)
(add-to-list 'package-selected-packages 'awk-ts-mode)
(add-to-list 'package-selected-packages 'bats-mode)
(add-to-list 'package-selected-packages 'xclip)
(add-to-list 'package-selected-packages 'evil-org)
(add-to-list 'package-selected-packages 'promise)
(add-to-list 'package-selected-packages 'exercism)
(add-to-list 'package-selected-packages 'evil-textobj-tree-sitter)
(add-to-list 'package-selected-packages 'hungry-delete)
(add-to-list 'package-selected-packages 'evil-surround)

;; judy-term
(if (member system-type '(windows-nt ms-dos))
    (add-to-list 'package-selected-packages 'powershell)
  (add-to-list 'package-selected-packages 'vterm))

;; judy-dev (also writing)
(add-to-list 'package-selected-packages 'let-alist)
(add-to-list 'package-selected-packages 'flymake-aspell)

(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'transient)
(add-to-list 'package-selected-packages 'xref)
(add-to-list 'package-selected-packages 'eldoc)
(add-to-list 'package-selected-packages 'puni)
(add-to-list 'package-selected-packages 'diff-hl)
(add-to-list 'package-selected-packages 'tabspaces)

;; Programming modes
(add-to-list 'package-selected-packages 'web-mode)
(add-to-list 'package-selected-packages 'yaml-mode)

;; (add-to-list 'package-selected-packages 'dockerfile-mode)
;; (add-to-list 'package-selected-packages 'glsl-mode)
;; (add-to-list 'package-selected-packages 'clang-format)
;; (add-to-list 'package-selected-packages 'cmake-mode)
;; (add-to-list 'package-selected-packages 'rust-mode)
;; (add-to-list 'package-selected-packages 'scad-mode)
;; (add-to-list 'package-selected-packages 'arduino-mode)
;; (add-to-list 'package-selected-packages 'arduino-cli-mode)

(unless (package-installed-p 'term-keys)
  (package-vc-install "https://github.com/junghan0611/term-keys"))

(unless (package-installed-p 'outli)
  (package-vc-install "https://github.com/jdtsmith/outli"))

;;; Install packages
(package-install-selected-packages :noconfirm)

;;; Load configuration

(global-unset-key (kbd "M-c"))  ; unset capitalize-word

(require 'crafted-defaults-config)
(require 'crafted-startup-config)
(require 'crafted-completion-config)
(require 'crafted-ui-config)

;; X (require 'crafted-evil-config)
(require 'core-evil)

(require 'crafted-ide-config)
(require 'crafted-lisp-config)
(require 'crafted-org-config)
(require 'crafted-writing-config)
(require 'crafted-package-config)
(require 'crafted-updates-config)
(require 'crafted-speedbar-config)

(customize-set-variable 'crafted-startup-module-list
                        '(crafted-startup-recentf crafted-startup-projects))

;; Custom modules
(require 'per-machine)
(require 'core-funcs)
(require 'judy-defaults)
(require 'judy-completion)
(require 'judy-evil)
(require 'judy-dev)

(require 'judy-org)
(require 'judy-term)
(require 'judy-fonts)
(require 'judy-theme)

(require 'judy-keys)

;; (require 'functions)
;; (require 'functions-1)

(require 'workspace)

(require 'hydra-config)

(require 'judy-transparency)
(judy-transparency-init 94)


(message "END")

;;; corkey bindings

(dolist (dir '("corkey" "corgi-bindings"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(message "Loading corgi-bindings...")
(require 'corgi-bindings)
;; Corgi's keybinding system, which builds on top of Evil. See the manual, or
;; visit the key binding and signal files (with `SPC f e k', `SPC f e K', `SPC
;; f e s' `SPC f e S')
;; Put this last here, otherwise keybindings for commands that aren't loaded
;; yet won't be active.

(message "Loading corkey...")
(require 'corkey)
(corkey-mode 1)
;; Automatically pick up keybinding changes
(corkey/load-and-watch)

;;; Dashboard

(message "Loading Dashboard...")

(when (= 1 (length (tab-bar-tabs)))
  (tab-bar-new-tab)
  (tab-bar-new-tab)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "Org" 1)
  (tab-bar-rename-tab "Note" 2)
  (tab-bar-rename-tab "Code" 3)
  (tab-bar-rename-tab "Emacs" 4)
  (tab-bar-select-tab 2)
  (dired denote-directory)
  (tab-bar-select-tab 3)
  (dired user-org-directory) ;; per-machine.el
  (tab-bar-select-tab 4)
  (find-file user-init-file)
  (delete-other-windows)
  (tab-bar-select-tab 1)
  ;; (delete-other-windows)
  ;; (org-agenda nil "a")
  )

;; install all language grammars hello

(setq treesit-auto-install 'prompt)
(crafted-ide-configure-tree-sitter)
;; install all language grammars, except protobuf

;;; _
(provide 'init)
;;; init.el ends here
