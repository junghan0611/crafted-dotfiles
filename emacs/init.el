;;; init.el --- Configuring Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Init loading up Crafted Emacs.

;;; Code:

;;; Custom file
(setq custom-file
      (expand-file-name ".cache/custom-vars.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil :nomessage))

;;; Bootstrap Crafted Emacs
(load (expand-file-name "modules/crafted-init-config.el" crafted-emacs-home))

;;; Configure packages to install
(require 'crafted-completion-packages)
;; (require 'crafted-evil-packages)
(require 'crafted-ide-packages)
(require 'crafted-lisp-packages)
(require 'crafted-org-packages)

(require 'crafted-ui-packages)
(require 'crafted-writing-packages)

;; (require 'crafted-workspaces-packages)

;;;; Additional packages for custom modules
;; judy-keys
(add-to-list 'package-selected-packages 'general)
(add-to-list 'package-selected-packages 'combobulate)
(add-to-list 'package-selected-packages 'which-key)
(add-to-list 'package-selected-packages 'pcre2el)
;; (add-to-list 'package-selected-packages 'doom-themes)
;; (add-to-list 'package-selected-packages 'ct)
;; (add-to-list 'package-selected-packages 'auto-dim-other-buffers)
(add-to-list 'package-selected-packages 'doom-modeline)
(add-to-list 'package-selected-packages 'winum)
;; (add-to-list 'package-selected-packages 'imenu-list)
;; (add-to-list 'package-selected-packages 'rainbow-mode)
;; (add-to-list 'package-selected-packages 'ansi-color)

(add-to-list 'package-selected-packages 'meow)
(add-to-list 'package-selected-packages 'hydra)
(add-to-list 'package-selected-packages 'major-mode-hydra) ; contains pretty-hydra

;; judy-theme
(unless (member 'modus-vivendi (custom-available-themes))
  (add-to-list 'package-selected-packages 'modus-themes))
(add-to-list 'package-selected-packages 'ef-themes)
;; (add-to-list 'package-selected-packages 'fontaine)
(add-to-list 'package-selected-packages 'keycast)

;; judy-evil
;; (add-to-list 'package-selected-packages 'evil-surround)

;; judy-term
(if (member system-type '(windows-nt ms-dos))
    (add-to-list 'package-selected-packages 'powershell)
  (add-to-list 'package-selected-packages 'vterm))

;; judy-dev (also writing)
(add-to-list 'package-selected-packages 'let-alist)
(add-to-list 'package-selected-packages 'flycheck)
(add-to-list 'package-selected-packages 'flymake-aspell)

(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'transient)
(add-to-list 'package-selected-packages 'xref)
(add-to-list 'package-selected-packages 'eldoc)
;; (add-to-list 'package-selected-packages 'smartparens)
(add-to-list 'package-selected-packages 'puni)
(add-to-list 'package-selected-packages 'diff-hl)
(add-to-list 'package-selected-packages 'tabspaces)

;; (add-to-list 'package-selected-packages 'org-roam)
;; (add-to-list 'package-selected-packages 'emacsql-sqlite-builtin)

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

;;; Install packages
(package-install-selected-packages :noconfirm)

;;; Load configuration

(require 'crafted-defaults-config)
(require 'crafted-completion-config)
;; (require 'crafted-evil-config)
(require 'crafted-ide-config)
;; install all language grammars
(crafted-ide-configure-tree-sitter)
;; (setq treesit-auto-install 'prompt)
;; install all language grammars, except protobuf
(require 'crafted-lisp-config)
(require 'crafted-org-config)

(customize-set-variable 'crafted-startup-module-list
                        '(crafted-startup-recentf crafted-startup-projects))

(require 'crafted-ui-config)
(require 'crafted-writing-config)
(require 'crafted-startup-config)

(require 'crafted-package-config)
(require 'crafted-updates-config)
(require 'crafted-speedbar-config)

;; Custom modules

(require 'core-funcs)

(require 'per-machine)

(require 'judy-completion)
(message "judy-completion")
(require 'judy-defaults)
(message "judy-defaults")
(require 'judy-dev)
(message "judy-dev")

;; (require 'judy-evil)
;; (message "judy-evil")

(require 'judy-org)
(message "judy-org")
(require 'judy-term)
(message "judy-term")
(require 'judy-fonts)
(message "judy-fonts")
(require 'judy-theme)
(message "judy-theme")

;; (require 'judy-keys)
;; (message "judy-keys")

;; (require 'functions)
;; (require 'functions-1)

(require 'workspace)

(require 'hydra-config)
(message "meow-config")
(require 'meow-config)
(message "meow-config")

(require 'judy-transparency)
(message "judy-transparency")
(judy-transparency-init 94)

(message "END")

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
  (find-file "~/.config/emacs/init.el")
  (delete-other-windows)
  (tab-bar-select-tab 1)
  (org-agenda nil "a")
  )

;;; _
(provide 'init)
;;; init.el ends here
