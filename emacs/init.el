;;; init.el --- Configuring Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Init loading up Crafted Emacs.

;;; Code:

;;; Custom file

(customize-set-variable 'crafted-startup-inhibit-splash t)

(setq custom-file
    (expand-file-name ".cache/custom-vars.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file nil :nomessage))

(customize-set-variable
    'package-archive-priorities
    '(("melpa"  . 99) ;; melpa first
         ("gnu"    . 90)   ; prefer GNU packages
         ("nongnu" . 80)   ; use non-gnu packages if not found in GNU elpa
         ("stable" . 70)   ; prefer "released" versions from melpa
         ))

;;; pinned-stable-packages

(defvar my/package-selected-packages-stable
    '(
         async
         ))

(customize-set-variable 'package-pinned-packages
    `(,@(mapcar (lambda (package) (cons package "stable"))
            my/package-selected-packages-stable)))

;;; Bootstrap Crafted Emacs
(load (expand-file-name "modules/crafted-init-config.el" crafted-emacs-home))

;;; Configure packages to install

(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-ide-packages)
(require 'crafted-org-packages)
(require 'crafted-ui-packages)
(require 'crafted-writing-packages)

;; (require 'crafted-lisp-packages) -> [X]

;;;; Additional packages for custom modules



;; melpa first
(defvar my/package-selected-packages
  '(
    aggressive-indent
    package-lint-flymake

    dash-functional
    treesit-auto

    which-key
    pcre2el
    minions

    winum
    avy
    string-edit-at-point
    expand-region
    goto-last-change
    imenu-list
    undo-fu
    tempel

    popper
    shackle

    hydra
    pretty-hydra
    major-mode-hydra

    magit
    transient ; melpa
    xref
    eldoc
    puni
    diff-hl
    tabspaces

    ;; Programming modes
    pipenv
    web-mode
    yaml-mode
    keycast
    apheleia
    bats-mode
    xclip

    org-contacts
    org-contrib
    org-project-capture
    org-pomodoro

    hungry-delete
    evil-org
    evil-surround
    evil-textobj-tree-sitter
    evil-escape

    citar
    citar-denote
    side-notes
    ))

(dolist (p my/package-selected-packages)
    (unless (package-installed-p p)
        (add-to-list 'package-selected-packages p 'append)))

;; judy-theme
(unless (member 'modus-vivendi (custom-available-themes))
    (add-to-list 'package-selected-packages 'modus-themes))

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

(unless (package-installed-p 'promise) ;; for exercism
    (package-vc-install "https://github.com/chuntaro/emacs-promise"))

(unless (package-installed-p 'exercism)
    (package-vc-install "https://github.com/anonimitoraf/exercism.el"))

(unless (package-installed-p 'term-keys)
    (package-vc-install "https://github.com/junghan0611/term-keys"))

(unless (package-installed-p 'orgabilize)
    (package-vc-install "https://github.com/akirak/orgabilize.el"))

(unless (package-installed-p 'outli)
    (package-vc-install "https://github.com/jdtsmith/outli"))

(unless (package-installed-p 'combobulate)
    (package-vc-install "https://github.com/mickeynp/combobulate"))

(unless (package-installed-p 'html-ts-mode)
    (package-vc-install "https://github.com/mickeynp/html-ts-mode"))

;; (unless (package-installed-p 'indent-bars)
;;     (package-vc-install "https://github.com/jdtsmith/indent-bars"))

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

;; (require 'crafted-evil-config) -- replaced core-evil
(require 'core-evil)

(require 'crafted-ide-config)
;; (crafted-ide-eglot-auto-ensure-all)

;; install all language grammars, except protobuf
(crafted-ide-configure-tree-sitter)

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

(require 'workspace)

(require 'hydra-config)

(require 'judy-transparency)
(judy-transparency-init 94)

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

;;; DONT Dashboard

(message "Loading Dashboard...")
(when (= 1 (length (tab-bar-tabs)))
  (tab-bar-new-tab)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "Note" 1)
  (tab-bar-rename-tab "Work" 2)
  (tab-bar-rename-tab "Emacs" 3)
  (tab-bar-select-tab 2)
  (dired user-org-directory) ;; per-machine.el
  (tab-bar-select-tab 3)
  (find-file user-init-file)
  (tab-bar-select-tab 1)
  (dired denote-directory)
  (delete-other-windows)
  ;; (org-agenda nil " ")
  (redraw-display)
  (tab-bar-select-tab 1)
  )

;;; load hydra-keys

(load-file (concat (file-name-as-directory user-emacs-directory) "hydrakeys.el"))

;;; _
(provide 'init)
;;; init.el ends here
