;;; judy-org.el --- org-mode and related module -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c i") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c \\") 'org-tags-sparse-tree)

;; Agenda does not have a key-binding by default.
;; Provide one as a starting point.
(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-directory user-org-directory)
(setq denote-directory (concat org-directory "roam/notes/"))
;; (setq org-roam-directory (concat org-directory "roam/"))

(require 'org-mode-crate)

(setq org-tag-alist
      '(("next" . ?x)
        ("notes" . ?n)
        ("important" . ?i)
        ("action_items" . ?a)
        ("joy" . ?j)
        ("waiting" . ?w)))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(setq org-workflow-directory (concat org-directory "roam/workflow/"))
(setq org-inbox-file (concat org-workflow-directory "20230202T020200--inbox__agenda.org"))
(setq org-default-notes-file org-inbox-file)
(setq org-user-agenda-files (list (concat org-directory "roam/workflow/")))
(setq org-user-agenda-diary-file (concat org-workflow-directory "20220101T010100--log__agenda.org"))

;; Set initial buffer to org
(setq initial-major-mode #'org-mode)

;; Some pretty settings
(setq org-pretty-entities nil
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

(setq org-export-coding-system 'utf-8)

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

(with-eval-after-load 'org-agenda
  (autoload #'evil-org-agenda-set-keys "evil-org-agenda" nil t)
  (evil-org-agenda-set-keys))

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (add-hook 'org-capture-after-finalize-hook #'evil-normal-state)
  )

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
;;; denote

(require 'denote)
(require 'denote-org-dblock)
(setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)

;; By default, we do not show the context of links.  We just display
;; file names.  This provides a more informative view.
(setq denote-backlinks-show-context t)

;; Pick dates, where relevant, with Org's advanced interface:
(setq denote-date-prompt-use-org-read-date nil)

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;; We use different ways to specify a path for demo purposes.
;; (setq denote-dired-directories
;;       (list denote-directory            ; The Zettelkasten directory
;;             ;; (thread-last denote-directory (expand-file-name "excerpts"))
;;             (thread-last denote-directory "excerpts")
;;             ;; (thread-last denote-directory (expand-file-name "attachments"))
;;             ;; (expand-file-name "~/Documents/books")
;;             ))

(setq denote-dired-directories
      (list denote-directory
            (concat denote-directory "excerpts/")))

(add-hook 'dired-mode-hook #'denote-dired-mode)

;; OR if only want it in `denote-dired-directories':
;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
(denote-rename-buffer-mode 1)

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(define-prefix-command 'denote-map)
(define-key global-map (kbd "C-c w") 'denote-map)
(let ((map denote-map))
  ;; (define-key map (kbd "n") #'denote)
  (define-key map (kbd "t") #'denote-type)
  (define-key map (kbd "T") #'denote-template)
  (define-key map (kbd "D") #'denote-date)
  (define-key map (kbd "z") #'denote-signature) ; "zettelkasten" mnemonic
  (define-key map (kbd "s") #'denote-subdirectory)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "l") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "L") #'denote-add-links)
  (define-key map (kbd "b") #'denote-backlinks)
  (define-key map (kbd "f f") #'denote-find-link)
  (define-key map (kbd "f b") #'denote-find-backlink)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "r") #'denote-region) ; "contents" mnemonic
  (define-key map (kbd "R") #'denote-rename-file-using-front-matter)
  (define-key map (kbd "M-r") #'denote-rename-file)

  (define-key map (kbd "k") #'denote-keywords-add)
  (define-key map (kbd "K") #'denote-keywords-remove)

  (define-key map (kbd "i") #'denote-org-dblock-insert-links)
  (define-key map (kbd "I") #'denote-org-dblock-insert-backlinks)
  )

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
  (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; https://emacs.stackexchange.com/questions/39434/evil-dont-yank-with-only-whitespace-to-register/53536#53536
(with-eval-after-load 'evil-org
  (define-key evil-normal-state-map "x" 'delete-forward-char)
  (define-key evil-normal-state-map "X" 'delete-backward-char)
  (evil-define-key 'normal 'evil-org-mode "x" 'delete-forward-char)
  (evil-define-key 'normal 'evil-org-mode "X" 'delete-backward-char)
  )

;;; binder side-notes

(require 'side-notes)
(add-hook 'side-notes-hook #'visual-line-mode) ; Good

(require 'binder)
(require 'binder-tutorial)

(require 'side-hustle)

(global-set-key (kbd "M-g b") 'binder-toggle-sidebar)
(global-set-key (kbd "M-g s") 'side-notes-toggle-notes) ;; M-s n
(global-set-key (kbd "M-g h") 'side-hustle-toggle)

;;; _
(provide 'judy-org)
;;; judy-org.el ends here
