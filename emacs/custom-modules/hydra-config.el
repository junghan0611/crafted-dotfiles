;;; hydra-config.el --- Configuring Emacs -*- lexical-binding: t; -*-

(require 'hydra)
(require 'major-mode-hydra)

(pretty-hydra-define hydra-denote (:title "Zettelkasten" :color blue)
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
   ))

(provide 'hydra-config)

;;;  ends here
