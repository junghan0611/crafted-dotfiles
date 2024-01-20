;;; workspace.el --- Configuring Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Init loading up Crafted Emacs.

;;; Code:

;;; tabspaces

(require 'tabspaces)

;; Make sure project is initialized

(setq tabspaces-use-filtered-buffers-as-default t) ; remap
(setq tabspaces-default-tab "Default")
(setq tabspaces-remove-to-default t)
(setq tabspaces-include-buffers '("*scratch*"))
(setq tabspaces-use-filtered-buffers-as-default t)

(setq tabspaces-initialize-project-with-todo t)
(setq tabspaces-todo-file-name "project-todo.org")

;; sessions
;; (tabspaces-session t)
;; (tabspaces-session-auto-restore t)
(setq tab-bar-new-tab-choice "*scratch*")

;; Filter Buffers for Consult-Buffer
(project--ensure-read-project-list)

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "*Tabspaces* Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

;;; tab-bar

(require 'tab-bar)

(unless (display-graphic-p) ; terminal
  (setq auto-resize-tab-bars nil) ; important
  (setq tab-bar-separator nil) ; important
  )

(setq auto-resize-tab-bars nil) ;; It works

(setq tab-bar-select-tab-modifiers '(control meta))
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-close-button-show nil)
(setq tab-bar-close-last-tab-choice nil)
(setq tab-bar-close-tab-select 'recent)
(setq tab-bar-new-tab-to 'right)
(setq tab-bar-position nil)
(setq tab-bar-show nil)
(setq tab-bar-tab-hints t) ; for tab-bar-circle-number

(setq tab-bar-tab-name-function 'tab-bar-tab-name-current) ; default

(setq tab-bar-format                    ; Emacs 28
      '(
        tab-bar-separator
        tab-bar-format-menu-bar
        tab-bar-format-tabs-groups
        tab-bar-separator
        ;; tab-bar-format-add-tab

        tab-bar-format-align-right
        tab-bar-format-global
        ))

(defun my/reload-tab-bar ()
  (interactive)

  (tab-bar-history-mode -1)
  (setq tab-bar-show nil)
  (tab-bar-mode -1)

  (tabspaces-mode t)

  (setq tab-bar-show t)
  (tab-bar-history-mode 1)

  (tab-bar-mode 1)
  (redraw-display)
  )

;; explicitly re-enable the cat for the first GUI client
;; 순서 상으로 먼저 탭바를 로드하고 테마를 로딩하는게 맞다.
(add-hook 'after-init-hook #'my/reload-tab-bar)

;;; provide

(provide 'workspace)

;;; init.el ends here
