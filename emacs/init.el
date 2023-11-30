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

;;; pinned-stable-packages

(defvar my/package-selected-packages-stable
  '(
    cider
    clojure-mode
    ))

(customize-set-variable 'package-pinned-packages
                        `(,@(mapcar
                             (lambda (package)
                               (cons package "stable"))
                             my/package-selected-packages-stable)))

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

;; melpa first
(defvar my/package-selected-packages
  '(
    ;; nerd-icons nerd-icons-dired nerd-icons-completion kind-icon
    dash-functional
    treesit-auto

    which-key
    pcre2el
    doom-modeline
    winum
    avy
    string-edit-at-point
    expand-region
    goto-last-change
    imenu-list
    undo-fu
    tempel
    neotree
    revert-buffer-all

    popper
    shackle

    hydra
    major-mode-hydra ; contains pretty-hydra

    ;; judy-dev (also writing)
    let-alist
    flymake-aspell

    magit
    transient
    xref
    eldoc
    puni
    diff-hl
    tabspaces

    ;; Programming modes
    web-mode
    yaml-mode
    keycast
    apheleia
    awk-ts-mode
    bats-mode
    xclip
    promise
    exercism

    org-contrib
    breadcrumb
    side-hustle

    hungry-delete
    evil-org
    evil-surround
    evil-textobj-tree-sitter
    evil-escape

    citar
    citar-denote
    binder
    side-notes

    zk zk-index zk-desktop zk-luhmann
    ))

(dolist (p my/package-selected-packages)
  (unless (package-installed-p p)
    (add-to-list 'package-selected-packages p 'append)))

;; judy-theme
(unless (member 'modus-vivendi (custom-available-themes))
  (add-to-list 'package-selected-packages 'modus-themes))

;; doom-themes
;; ct
;; auto-dim-other-buffers
;; rainbow-mode

(unless *is-termux*
  (add-to-list 'package-selected-packages 'nerd-icons)
  (add-to-list 'package-selected-packages 'nerd-icons-dired)
  (add-to-list 'package-selected-packages 'nerd-icons-completion)
  (add-to-list 'package-selected-packages 'kind-icon)
  )

;; judy-term
(if (member system-type '(windows-nt ms-dos))
    (add-to-list 'package-selected-packages 'powershell)
  (add-to-list 'package-selected-packages 'vterm))

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

;; zk packages
;; (unless (package-installed-p 'zk-luhmann)
;;   ;; (package-vc-install "https://github.com/localauthor/zk")
;;   (package-vc-install "https://github.com/junghan0611/zk-luhmann")
;;   (package-vc-install "https://github.com/localauthor/link-hint-preview")
;;   )

(unless (package-installed-p 'orgabilize)
  (package-vc-install "https://github.com/akirak/orgabilize.el"))

;; (unless (package-installed-p 'org-mode-crate)
;;   (package-vc-install "https://github.com/junghan0611/org-mode-crate"))

(unless (package-installed-p 'outli)
  (package-vc-install "https://github.com/jdtsmith/outli"))

;; (unless (package-installed-p 'outli)
;;   (package-vc-install "https://github.com/jdtsmith/outli"))

;; emacs-verson > 30
(when (version< "30" emacs-version)
  ;; Get some Emacs 29 compatibility functions. Notably missing is
  ;; `setopt' which the `compat' library deliberately does not
  ;; provide, so we continue to use the `customize-set-variable'
  ;; function for setting user options, unless we have a version guard
  ;; around a block, in which case we use `setopt' instead.
  (package-vc-install "https://github.com/junghan0611/compat"))

;;; Install packages
(package-install-selected-packages :noconfirm)

;;; Load configuration

(global-unset-key (kbd "M-c"))  ; unset capitalize-word

(require 'crafted-defaults-config)
(remove-hook 'text-mode-hook #'flyspell-mode)
(remove-hook 'prog-mode-hook #'flyspell-prog-mode)
(repeat-mode -1)

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
;; (crafted-ide-configure-tree-sitter)
;; install all language grammars, except protobuf

;;; _
(provide 'init)
;;; init.el ends here
