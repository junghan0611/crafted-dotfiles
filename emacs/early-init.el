;;; early-init.el --- Bootstrapping Crafted Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Early init.
;; Set up deferred native compilation, dark-theme by default and GC opts.
;; Bootstrap Crafted Emacs early-init.

;;; Code:

;;; Garbage Collection/Startup Message
(setq gc-cons-threshold most-positive-fixnum)

(defun my/post-startup ()
  "Sets GC threshold and displays load-time message."
  (setq gc-cons-threshold (* 20 1000 1000))
  (message "Emacs loaded (%s seconds)" (emacs-init-time "%.2f")))

(add-hook 'emacs-startup-hook #'my/post-startup)

;;; Native Compilation
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name ".cache/eln-cache/"
                                 user-emacs-directory)))

;;; Crafted Emacs
(defconst my/crafted-emacs-branch "craftedv2RC1"
  "Branch to clone (does not update branch if already cloned).")

(defvar crafted-emacs-home
  (expand-file-name "crafted-emacs"
                    (file-name-directory load-file-name))
  "Crafted Emacs Home (overwritten in early-init.el).")

;; Ensure crafted-emacs-home exists
(make-directory crafted-emacs-home t)

(when (directory-empty-p crafted-emacs-home)
  (message "Cloning crafted-emacs ...")
  (shell-command-to-string
   (format "git clone https://github.com/jvdydev/crafted-emacs -b %s %s"
           my/crafted-emacs-branch
           crafted-emacs-home)))

(load (expand-file-name "modules/crafted-early-init-config.el"
                        crafted-emacs-home))

;;; Load a dark theme to avoid flashing on load
;; On Emacs 28+: Modus Themes is built-in and loaded, otherwise deeper-blue
(if (member 'modus-vivendi (custom-available-themes))
    (load-theme 'modus-vivendi t)
  (load-theme 'deeper-blue t))

;;; _
(provide 'early-init)
;;; early-init.el ends here