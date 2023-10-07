;;; judy-org.el --- org-mode and related module -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(message "1")

;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c i") 'org-insert-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c \\") 'org-tags-sparse-tree)

(setq org-directory user-org-directory)
(setq denote-directory (concat org-directory "denote/"))
;; (setq org-roam-directory (concat org-directory "roam/"))

(setq org-workflow-directory (concat org-directory "workflow/"))
(setq org-inbox-file (concat org-workflow-directory "inbox.org"))
(setq org-default-notes-file (concat org-workflow-directory "inbox.org"))
(setq org-user-agenda-files (list (concat org-directory "workflow/")))

;; Set initial buffer to org
(setq initial-major-mode #'org-mode)

(message "2")

;; Some pretty settings
(setq org-pretty-entities t
      org-hide-emphasis-markers t
      org-hide-block-startup nil
      org-fontify-quote-and-verse-blocks t
      org-edit-src-content-indentation 0
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t
      org-src-window-setup 'current-window)

;; fast src blocks etc.
;; (require 'org-tempo)

;; ;; Load babel languages for evaluating blocks
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (C . t)
;;    (shell . t)
;;    (latex . t)
;;    (lisp . t)
;;    (org . t)
;;    (clojure . t)
;;    (R . t)
;;    (scheme . t)))

;; ;;; org-export
;; (require 'ox-md)
;; (require 'ox-beamer)
;; (require 'ox-texinfo)

;; (customize-set-variable 'org-export-coding-system 'utf-8)

;; ;; set up LaTeX export to work with minted package
;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted"))
;;       org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(message "3")

;;;  org-agenda

(require 'org-agenda)

;; The following setting is different from the document so that you
;; can override the document org-agenda-files by setting your
;; org-agenda-files in the variable org-user-agenda-files
(if (boundp 'org-user-agenda-files)
    (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/org/inbox.org"))))

(if (boundp 'org-user-agenda-diary-file)
    (setq org-agenda-diary-file org-user-agenda-diary-file)
  (setq org-agenda-diary-file "~/org/diary.org"))

(setq diary-file org-agenda-diary-file)

(message "4")

;;; org-roam

;; (require 'org-roam)
;; (require' emacsql-sqlite-builtin)
;; (setq org-roam-database-connector 'sqlite-builtin)

;; (setq org-roam-index-file (concat org-roam-directory "_index.org"))

;; ;; Navigation in the backlink buffer is intuitive (use RET, C-u RET).
;; ;; If org-roam-visit-thing does not work for you, this below might:
;; (define-key org-roam-mode-map [mouse-1] #'org-roam-preview-visit)

;; (setq org-roam-file-exclude-regexp '("temp/" "layers/" "reveal-root/" "attach/" "oldseq/" "data/" "archive/" "\\<todo\\.org" "\\<people\\.org" "\\<index\\.org" "\\<OSS\\.org" "\\<habits\\.org"
;;                                      ))
;; ;; https://www.orgroam.com/manual.html#Customizing-Node-Caching
;; (setq org-roam-db-node-include-function
;;       (lambda ()
;;         (not (member "ATTACH" (org-get-tags)))))

;; (setq org-roam-db-gc-threshold most-positive-fixnum)
;; (setq org-roam-v2-ack t)

;; (setq org-roam-node-display-template
;;       (concat
;;        (propertize "${directories:10} " 'face 'org-checkbox)
;;        (propertize "${hierarchy:80} " 'face 'org-roam-title)
;;        (propertize "${backlinkscount:5} " 'face 'org-formula)
;;        (propertize "${tags:40}" 'face 'org-tag))
;;       org-roam-node-annotation-function
;;       (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))

;; ;; https://github.com/org-roam/org-roam/wiki/User-contributed-Tricks#showing-node-hierarchy
;; (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
;;   (let ((level (org-roam-node-level node)))
;;     (concat
;;      (when (> level 0) (concat (org-roam-node-file-title node) " > "))
;;      (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
;;      (org-roam-node-title node))))

;; ;; ;; suggested keymap based on example from project documentation
;; (keymap-global-set "C-c r l" #'org-roam-buffer-toggle)
;; (keymap-global-set "C-c r f" #'org-roam-node-find)
;; (keymap-global-set "C-c r g" #'org-roam-graph)
;; (keymap-global-set "C-c r i" #'org-roam-node-insert)
;; (keymap-global-set "C-c r c" #'org-roam-capture)
;; ;; (keymap-global-set "C-c r j" . org-roam-dailies-capture-today)

;; ;; ;; Enable automatic sync of the SQLite database
;; (org-roam-db-autosync-mode)

;;; _
(provide 'judy-org)
;;; judy-org.el ends here
