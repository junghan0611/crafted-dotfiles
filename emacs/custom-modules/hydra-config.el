;;; hydra-config.el --- Configuring Emacs -*- lexical-binding: t; -*-

;;; major-mode-hydra

(require 'hydra)
(require 'major-mode-hydra)
(require 'all-the-icons)

;; Before hydra because we use pretty-hydra-define in the hydra confg.
(setq major-mode-hydra-invisible-quit-key "q")
(setq major-mode-hydra-title-generator
    #'(lambda (mode)
          (s-concat "\n"
              (s-repeat 10 " ")
              (all-the-icons-icon-for-mode mode :v-adjust 0.05)
              " "
              (symbol-name mode)
              " commands")))

;; Mode maps
(major-mode-hydra-define org-mode
    (:title "Org-mode" :color amaranth :separator "=" :quit-key "<escape>")
    ("Movement"
        (("u" org-up-element "up" :exit nil)
            ("n" org-next-visible-heading "next visible heading" :exit nil)
            ("p" org-previous-visible-heading "previous visible heading" :exit nil)
            ("l" org-next-link "next link" :exit nil)
            ("L" org-previous-link "previous link" :exit nil)
            ("b" org-next-block "next block" :exit nil)
            ("B" org-prev-block "previous block" :exit nil)
            ("g" org-mark-ring-goto "pop mark" :exit nil))
        "Subtrees" (("k" org-cut-subtree "kill")
                       (">" org-demote-subtree "demote" :exit nil)
                       ("<" org-promote-subtree "promote" :exit nil)
                       ("N" org-narrow-to-subtree "narrow")
                       ("r" org-refile "refile")
                       ("." org-tree-to-indirect-buffer "indirect buffer")
                       ("'" org-id-get-create "create id"))
        "Inserting" (("c" citar-insert-citation "insert citation")
                        ("e" org-expiry-insert-expiry "insert expiry property")
                        ("i" org-insert-heading-respect-content "insert heading")
                        ("y" ash/org-paste-link "yank link" :exit t))
        "Opening" (("o" org-open-at-point "open at point"))
        "Clock" (("P" org-pomodoro "Start pomodoro")
                    ("Q" ash/org-pomodoro-til-meeting "Start pomodoro til half hour"))
        )
    )

(major-mode-hydra-define emacs-lisp-mode
    (:title "Emacs-Lisp-mode" :color blue :separator "=" :quit-key "<escape>")
    ("Eval"
        (("b" eval-buffer "eval buffer")
            (";" eval-expression "eval expression")
            ("d" eval-defun "eval defun")
            ("D" edebug-defun "edebug defun")
            ("e" eval-last-sexp "eval last sexp")
            ("E" edebug-eval-last-sexp "edebug last sexp")
            ("l" ielm "ielm"))
        "Test"
        (("t" ert "prompt")
            ("T" (ert t) "all")
            ("F" (ert :failed) "failed"))
        "Doc"
        (("f" describe-function "function")
            ("v" describe-variable "variable")
            ("i" info-lookup-symbol "info lookup")))
    )

(major-mode-hydra-define python-ts-mode
    (:title "python-ts-mode" :color pink :separator "=" :quit-key "<escape>")
    ("Movement"
        (("o" combobulate "combobulate" :exit t)))
    )

(major-mode-hydra-define clojure-mode
    (:title "Clojure-mode" :color pink :separator "=" :quit-key "<escape>")
    ("Load"
        (("k" cider-load-buffer "buffer" :exit nil)
            ("l" cider-load-file "file" :color red))))

(major-mode-hydra-define eshell-mode
    (:title "Eshell-mode" :color pink :separator "=" :quit-key "<escape>")
    ("Movement"
        (("h" consult-history "history" :exit t))))

;;;;; Hydra All

;; define everything here
(require 'pretty-hydra)
(require 's)
(require 'all-the-icons)

;; with-faicon function allows an icon in hydra title. Requires following requires and aliases. To omit don't include 'with-faicon' in appearance-title
;; define an icon function with all-the-icons-faicon
;; to use filecon, etc, define same function with icon set
(defun with-faicon (icon str &rest height v-adjust)
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
(defun with-fileicon (icon str &rest height v-adjust)
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;;;;;; hydra-jumps

(pretty-hydra-define hydra-jumps
    (:color amaranth :exit t :quit-key "q")
    ("Jump visually"
        (("j" avy-goto-word-1 "to word" :exit t)
            ("l" avy-goto-line "to line" :exit t)
            ("c" avy-goto-char "to char" :exit t)
            ("r" avy-resume "resume" :exit t))
        "Jump via minibuffer"
        (("i" consult-imenu "imenu" :exit t)
            ("o" consult-outline "outline" :exit t))
        "Jump & go"
        (("u" ash/avy-goto-url "open url" :exit t))
        "Misc"
        (("=" hydra-all/body "back" :exit t))))

;;;;;; hydra-structural

  ;;;;; hydra-structural : puni
(pretty-hydra-define hydra-structural
    (:color amaranth :quit-key "q")
    ("Change"
        (
            ("," puni-slurp-forward "slurp-forward")
            ("." puni-barf-forward "barf-forward")
            ("]" puni-slurp-forward "slurp-backward")
            ("[" puni-barf-forward "barf-backward")
            ("." puni-splice "splice")
            ("?" puni-convolute "convolute"))
        "Movement"
        (("a" puni-beginning-of-sexp "beginning of sexp")
            ("e" puni-end-of-sexp "end of sexp")
            (")" puni-syntactic-forward-punc "down sexp")
            ("(" puni-syntactic-backward-punc "up sexp"))
        "Formatting"
        (("z" puni-squeeze "squeeze/unwrap"))
        "Misc"
        (("=" hydra-all/body "back" :exit t))))

;;;;;; hydra-multiple-cursors

(pretty-hydra-define hydra-multiple-cursors
    (:color amaranth :quit-key "q")
    ("Mark via region"
        (("l" mc/edit-lines "edit lines" :exit t)
            ("s" mc/mark-all-in-region-regexp "mark all in region re" :exit t))
        "Mark"
        (("a" mc/mark-all-like-this "mark all" :exit t)
            ("d" mc/mark-all-dwim "mark dwim" :exit t))
        "Mark incrementally"
        (("n" mc/mark-next-like-this "mark next like this")
            ("N" mc/skip-to-next-like-this "skip to next like this")
            ("M-n" mc/unmark-next-like-this "unmark next like this")
            ("p" mc/mark-previous-like-this "mark previous like this")
            ("P" mc/skip-to-previous-like-this "skip to previous like this")
            ("M-p" mc/unmark-previous-like-this "unmark previous like this")
            ("L" mc/mark-next-lines "mark next lines"))
        "Insert"
        (("0" mc/insert-numbers "insert numbers" :exit t)
            ("A" mc/insert-letters "insert letters" :exit t))
        "Misc"
        (("=" hydra-all/body "back" :exit t))))

;;;;;; DONT hydra-expand

;; (pretty-hydra-define hydra-expand
;;   (:color amaranth :quit-key "q")
;;   ("Expand/Contract"
;;    (("e" er/expand-region "expand")
;;     ("c" er/contract-region "contract"))
;;    "Expand to..."
;;    (("d" er/mark-defun "defun")
;;     ("\"" er/mark-inside-quotes "quotes")
;;     ("'" er/mark-inside-quotes "quotes")
;;     ("p" er/mark-inside-pairs "pairs")
;;     ("." er/mark-method-call "call"))
;;    "Misc"
;;    (("=" hydra-all/body "back" :exit t))))

;;;;;; hydra-denote

(pretty-hydra-define hydra-denote
    (:color amaranth :exit t :quit-key "q"
        :pre (progn (setq which-key-inhibit t))
        :post (progn (setq which-key-inhibit nil)))
    ("new"
        (("n" denote-signature "new")
            ("d" denote-date "new with date"))
        "link"
        (("i" denote-link "insert link")                  ;mnemonic "insert"
            ("c" denote-link-after-creating "link to new"))  ;mnemonic "create"
        "inspect & open"
        (("l" denote-link-find-file "open linked file")   ;mnemonic "link"
            ("b" denote-link-find-backlink "open backlink")  ;mnemonic "backlink"
            ("s" consult-notes "search zettels")
;("f" (consult-ripgrep denote-directory) "full text")
            ("f" consult-notes-search-in-all-notes "full text"))
        "modify"
        (("r" denote-rename-file "rename")
            ("u" denote-rename-file-using-front-matter "update name"))
        )
    )

;;;;;; hydra-yas

(pretty-hydra-define hydra-yas ()
    ("Snippets"
        (("n" yas-new-snippet "new" :exit t)
            ("r" yas-reload-all "reload" :exit t)
            ("v" yas-visit-snippet-file "visit" :exit t))
        "Movement"
        (("f" yas-next-field "forward field" :exit nil)
            ("b" yas-prev-field "previous field" :exit nil))))

;;;;;; hydra-flymake

(pretty-hydra-define hydra-flymake   ()
    ("Movement"
        (("n" flymake-goto-next-error "next error")
            ("p" flymake-goto-prev-error "previous error")
            ("d" flymake-goto-diagnostic "diagnostic")
            ("." consult-flymake))
        "Display"
        (("." flymake-show-diagnostic "show diagnostic")
            ("B" flymake-show-diagnostics-buffer "diagnostics buffers"))
        "Misc"
        (("=" hydra-all/body "back" :exit t))))

;;;;;; hydra-org-main

(pretty-hydra-define hydra-org-main ()
    ("Misc"
        (("a" org-agenda "agenda" :exit t)
            ("c" org-capture "capture" :exit t))
        "Links"
        (("s" org-store-link "store" :exit t))))

;;;;;; hydra-find

(pretty-hydra-define hydra-find ()
    ("In-Buffer"
        (("i" consult-imenu "imenu" :exit t)
            ("m" consult-mark "mark rings" :exit t)
            ("o" consult-multi-occur "occur" :exit t)
            ("e" consult-flymake "errors" :exit t)
            ("l" consult-goto-line "line" :exit t))
        "Other"
        (("r" consult-ripgrep "grep" :exit t)
            ("b" consult-bookmark "bookmark" :exit t)
            ("R" consult-register "register" :exit t)
            ("C" consult-complex-command "complex command" :exit t))))

;;;;;; hydra-all

(pretty-hydra-define hydra-all
    (:quit-key "<escape>" :title "All")
    ("Applications"
        (
            ;; ("m" hydra-mail/body "mail" :exit t)
            ("o" hydra-org-main/body "org" :exit t)
            ("d" hydra-denote/body "denote" :exit t)
            ;; ("S" hydra-straight/body "straight" :exit t)
            ;; ("!" ash/el-secretario-daily-review "secretary" :exit t)
            ("g" magit-status "magit" :exit t))
        "Editing"
        (("c" hydra-multiple-cursors/body "multiple cursors" :exit t)
            ("s" hydra-structural/body  "structural" :exit t)
            ;; ("e" hydra-expand/body "expand region" :exit t)
            ("y" hydra-yas/body "snippets" :exit t))
        "Movement"
        (("j" hydra-jumps/body "jumps" :exit t)
            ("E" hydra-flymake/body "errors" :exit t)
            ;; ("G" deadgrep "grep" :exit t)
            )
        "Misc"
        (
            ;; ("t" hydra-toggles/body "toggles" :exit t)
            ("q" nil "Quit" :color red :exit t)
            ("f" hydra-find/body "find" :exit t))))

;;;;; hydra-jump-to-directory

(defhydra hydra-jump-to-directory
    (:color amaranth :exit t :quit-key "<escape>")
    "Jump to directory"

    ("b" (find-file "~/git/blog") "blog")
    ("n" (find-file "~/git/notes") "notes")
    ("c" (find-file "~/nosync/clone-notes/") "clone-notes")
    ("C" (find-file "~/sync/markdown/cheat") "cheat")
    ("m" (find-file "~/sync/man") "man")
    ("o" (find-file "~/sync/org/") "org")
    ("s" (find-file "~/.spacemacs.d/snippets/") "snippets")

    ("v" (find-file "~/Videos") "Videos")
    ("p" (find-file "~/Pictures") "Pictures")
    ("d" (find-file "~/Documents") "Documents")
    ("D" (find-file "~/Downloads") "Downloads")
    ("P" (find-file "~/Public") "Public")
    ("t" (find-file "~/Templates") "Templates")
    ("q" nil "Quit" :color blue))

;;;;; hydra-jump-to-system-file

(defhydra hydra-jump-to-system-file
    (:color amaranth :exit t :quit-key "<escape>")
    "Jump to system file"

    ("b" (find-file "~/.bashrc") "bashrc")
    ("z" (find-file "~/.zshrc") "zshrc")
    ("u" (find-file "~/sync/elfeed/elfeed.org") "elfeed.org")
    ("q" nil "Quit" :color blue))

(global-set-key (kbd "<f1>") 'hydra-all/body)
(global-set-key (kbd "<f2>") 'major-mode-hydra)

(provide 'hydra-config)

;;;  ends here
