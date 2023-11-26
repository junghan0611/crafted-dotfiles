;;; early-init.el --- Bootstrapping Crafted Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Early init.
;; Set up deferred native compilation, dark-theme by default and GC opts.
;; Bootstrap Crafted Emacs early-init.

;;; Code:

;;; Defvar OS

(defvar *is-mac*     (eq system-type 'darwin))
(defvar *is-windows* (eq system-type 'windows-nt))
(defvar *is-cygwin*  (eq system-type 'cygwin))
(defvar *is-linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defvar *is-wsl*     (eq (string-match "Linux.*microsoft.*WSL2.*Linux" (shell-command-to-string "uname -a")) 0))
(defvar *is-unix*    (or *is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))
(defvar *is-android*  (eq system-type 'android))
(defvar *is-termux*
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

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
;; (defconst my/crafted-emacs-branch "craftedv2RC1"
;; (defconst my/crafted-emacs-branch "master"
  (defconst my/crafted-emacs-branch "ko"
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
   ;; (format "git clone https://github.com/SystemCrafters/crafted-emacs -b %s %s"
   ;; (format "git clone https://github.com/jvdydev/crafted-emacs -b %s %s"
   (format "git clone https://github.com/junghan0611/crafted-emacs -b %s %s"
           my/crafted-emacs-branch
           crafted-emacs-home)))

(load (expand-file-name "modules/crafted-early-init-config.el"
                        crafted-emacs-home))

;;; is-android

(when *is-android*
  (message "Loading Android Emacs\n")
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin" (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (format "%s:%s" "/data/data/com.termux/files/usr/lib" (getenv "LD_LIBRARY_PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))

;;; Load a dark theme to avoid flashing on load
(if (member 'modus- (custom-available-themes))
    (load-theme 'modus-operandi t)
  (load-theme 'deeper-blue t))

;;; _
(provide 'early-init)
;;; early-init.el ends here
