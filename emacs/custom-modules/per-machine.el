;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

;;; User Profile

(setq user-full-name (if (getenv "USER_FULL_NAME")
                         (getenv "USER_FULL_NAME")
                       "John Doe"))

(setq user-mail-address (if (getenv "USER_MAIL_ADDRESS")
                            (getenv "USER_MAIL_ADDRESS")
                          "john.doe@example.com"))

;; Set my GPG key as the default key
(setq-default epa-file-encrypt-to (if (getenv "EPA_FILE_ENCRYPT_TO")
                                      (list (getenv "EPA_FILE_ENCRYPT_TO"))
                                    (list "ABCDEFGHIJKLMN")))

;;; Path

(defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                 (getenv "ORG_DIRECTORY")
                               "~/org/"))

;;; _
(provide 'per-machine)

;;; per-machine.el ends here
