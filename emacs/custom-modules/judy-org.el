;;; judy-org.el --- org-mode and related module -*- lexical-binding: t; -*-
;;; Commentary:

;;; load org-mode.el

(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f9>"))

(setq org-crypt-key "B5ADD9F47612A9DB") ; junghanacs

;;; whhone

;;;; Miscellaneous

;; Use year/month/day
(setq calendar-date-style 'iso)
(xterm-mouse-mode +1)
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;;; user-org-directory

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
    (setq org-directory "~/org/"))

(message "`org-directory' has been set to: %s" org-directory)

;;; paste

;;;;; ddd
(setq org-enforce-todo-dependencies t)

(setq org-cycle-separator-lines 0)

;; (setq org-blank-before-new-entry (quote ((heading)
;;                                          (plain-list-item . auto))))
(setq org-insert-heading-respect-content nil)

;; 리버스 순서가 익숙하다.
(setq org-reverse-note-order t) ; default nil

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
;; (setq org-show-siblings (quote ((default))))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
    (interactive)
    (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
    (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
    (interactive)
    (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
    (save-excursion
        (when bh/insert-inactive-timestamp
            (org-return)
            (org-cycle)
            (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

(setq org-return-follows-link t)

(setq org-tags-match-list-sublevels t)
(setq org-agenda-persistent-filter t)

;; Bookmark handling
;; (global-set-key (kbd "<C-f6>") #'(lambda () (interactive) (bookmark-set "SAVED")))
;; (global-set-key (kbd "<f6>") #'(lambda () (interactive) (bookmark-jump "SAVED")))

(setq org-src-preserve-indentation t)

(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq org-time-clocksum-format
    '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; (setq org-use-sub-superscripts nil)
;; (setq org-odd-levels-only nil)
;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; ex) 2022-09-19 (월)
(setq org-agenda-format-date "%Y-%m-%d (%a)")

(defun bh/make-org-scratch ()
    (interactive)
    (find-file "/tmp/publish/scratch.org")
    (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

;; Custom Key Bindings
(global-set-key (kbd "<f9> c") 'calendar)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)

;; Adding a "/" so that =find-file= finds the files under =~/org/=.
;;
;; The org-directory is computed based on user-emacs-directory.
;; - ".emacs.d" -> "~/org/"
;; - ".emacs.d-personal" -> "~/org-personal/"
;;(concat "~/org" (nth 1 (split-string user-emacs-directory "emacs.d"))))
;; (setq org-directory
;;       (concat "~/org" (nth 1 (split-string dotspacemacs-directory
;;                                            "spacemacs.d"))))

(defun my/expand-org-file-name (filename)
    (expand-file-name filename org-directory))

(defun my/org-inbox-file () (my/expand-org-file-name "agenda/20230202T020200--inbox.org"))
(defun my/org-tasks-file () (my/expand-org-file-name "agenda/20230101T010100--tasks.org"))
(defun my/org-notes-file () (my/expand-org-file-name "agenda/20230219T035500--notes.org"))
(defun my/org-diary-file () (my/expand-org-file-name "agenda/20220101T010100--diary.org"))
(defun my/org-drill-file () (my/expand-org-file-name "agenda/20240124T164402--drill.org"))
(defun my/org-blog-file () (my/expand-org-file-name "20240104T061355--blog.org"))
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attachment/"))
(defun my/org-emacs-config-file () (expand-file-name "jh-emacs.org" user-emacs-directory))


;;;;; Capturing

;; The default file for capturing.
(setq org-default-notes-file (my/org-inbox-file))

;; Org Capture Templates
;; See https://orgmode.org/manual/Template-elements.html#index-org_002ddefault_002dnotes_002dfile-1
(setq org-capture-templates nil)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
;; (setq org-capture-templates
;;       (quote (("t" "todo" entry (file org-refile-file)
;;                "* TODO [#C] %?\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("r" "respond" entry (file org-refile-file)
;;                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
;;               ("n" "note" entry (file org-refile-file)
;;                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("w" "org-protocol" entry (file org-refile-file)
;;                "* TODO Review %c\n%U\n" :immediate-finish t)
;;               ("m" "Meeting" entry (file org-refile-file)
;;                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;;               ("h" "Phone call" entry (file org-refile-file)
;;                "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;;               ("H" "Habit" entry (file org-refile-file)
;;                "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; One-click Capture for Tasks. Captures the task immediately and gets out of your way.
;; (push `("T" "Todo Immediate Finish" entry
;;         (file+headline org-refile-file "FleetBox")
;;         "* TODO [#C] %^{Todo title}\n%t\n%a\n%?"
;;         ;; :clock-in t :clock-resume t
;;         :immediate-finish t)
;;       org-capture-templates)

(add-to-list
    'org-capture-templates
    `("i" "Inbox" entry (file ,(my/org-inbox-file))
         "* %?\n%i\n%a"))

;; TODO fix
(add-to-list
    'org-capture-templates
    `("p" "Project" entry (file ,(my/org-tasks-file))
         "* %?\n%i\n%a"))

;; (add-to-list
;;  'org-capture-templates
;;  `("p" "Project" entry (file+headline ,(my/org-tasks-file) "Projects")
;;    (file "~/org/.template/project.org")))

(add-to-list
    'org-capture-templates
    `("v" "Vocab" entry (file+headline ,(my/org-drill-file) "Translation")
         "* %? :drill:\n\n** Translation\n\n** Definition\n"))

;;;;; org-capture-templates -- org-log-file

(setq org-datetree-add-timestamp t)

(push `("j" "Journal" entry (file+olp+datetree ,(my/org-diary-file))
           "* %<%H:%M> - %?\n%U\n" :clock-in t :clock-resume t) ; remove %a
    org-capture-templates)
;; :empty-lines 1 :prepend t -- 역순 등록

(message "Press `C-c a' to get started with your agenda...")

(require 'org-attach)

(setq org-attach-id-dir (my/org-attachment-directory))
(setq org-attach-use-inheritance t)

;; PROG -> DOING 이 적절할 것 같은데?!

(setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-custom-commands
    '(("n" "Agenda / PROG / TODO"
          ((agenda "" nil)
              (todo "PROG" nil)
              (todo "NEXT" nil))
          nil)))

;; Sets default-directory to org-directory so that =M-x magit= from the agenda view does not ask me for a dir.
(global-set-key (kbd "C-c a")
    (lambda ()
        (interactive)
        (let ((default-directory org-directory)) (org-agenda))))

;; My agenda files.
;; (my/org-calendar-directory
(setq org-agenda-files (list (my/org-diary-file) (my/org-inbox-file) (my/org-tasks-file) (my/org-notes-file)))

;; Trying to use the current window as agenda frame.
(setq org-agenda-window-setup 'current-window)

;; Use sticky agenda since I need different agenda views (personal and work) at the same time.
(setq org-agenda-sticky t)

;; Just today
(setq org-agenda-span 'day)

;; Hide all scheduled todo.
(setq org-agenda-todo-ignore-scheduled 'all)

;; Ignores "far" deadline TODO items from TODO list.
(setq org-agenda-todo-ignore-deadlines 'far)

;; Hide all scheduled todo, from tags search view, like tags-todo.
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Hide all done todo in agenda
(setq org-agenda-skip-scheduled-if-done t)

;; Hide task until the scheduled date.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

;; Use an indirect buffer after <Tab> (org-agenda-goto) or <Enter> (org-agenda-switch-to).
;;
;; Also see https://emacs.stackexchange.com/a/17822
;; (advice-add 'org-agenda-goto :after
;;             (lambda (&rest args)
;;               (org-tree-to-indirect-buffer)))
;; (advice-add 'org-agenda-switch-to :after
;;             (lambda (&rest args)
;;               (org-tree-to-indirect-buffer)))

;; Narrow to subtree after <Tab> (org-agenda-goto) or <Enter> (org-agenda-switch-to).
;; (advice-add 'org-agenda-goto :after
;;             (lambda (&rest args)
;;               (org-narrow-to-subtree)))
;; (advice-add 'org-agenda-switch-to :after
;;             (lambda (&rest args)
;;               (org-narrow-to-subtree)))

(setq org-enforce-todo-dependencies t)

(setq org-log-into-drawer t)
;; (setcdr (assoc 'note org-log-note-headings) "%d")
;; Interstitial Journaling: add note to CLOCK entry after clocking out
;; https://emacs.stackexchange.com/questions/37526/add-note-to-clock-entry-after-clocking-out
(setq org-log-note-clock-out t)

;; 4 priorities to model Eisenhower's matrix.
;; - [#A] means +important +urgent
;; - [#B] means +important -urgent
;; - [#C] means -important +urgent
;; - [#D] means -important -urgent
(setq org-priority-default 68
    org-priority-lowest 68)

;; Include the file name into the path in refile target.
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets
    `((nil :maxlevel . 3)
         (,(my/org-tasks-file) :maxlevel . 2)
         (,(my/org-notes-file) :maxlevel . 3)))
(setq org-outline-path-complete-in-steps nil)

(defun my/consult-org-agenda ()
    (interactive)
    (consult-org-agenda)
    (org-tree-to-indirect-buffer))

(setq my/consult-org-files '())
(add-to-list 'my/consult-org-files (my/org-inbox-file) t)
(add-to-list 'my/consult-org-files (my/org-tasks-file) t)
(add-to-list 'my/consult-org-files (my/org-notes-file) t)
(when (file-exists-p (my/org-blog-file))
    (add-to-list 'my/consult-org-files (my/org-blog-file) t))
(when (file-exists-p (my/org-emacs-config-file))
    (add-to-list 'my/consult-org-files (my/org-emacs-config-file) t))

(defun my/consult-org-all ()
    (interactive)
    (consult-org-heading
        "+LEVEL<=3"
        my/consult-org-files))

(defun my/consult-org-notes ()
    (interactive)
    (consult-org-heading
        "+LEVEL<=3"
        (list (my/org-notes-file))))

(global-set-key (kbd "C-S-j") #'my/consult-org-all)
(global-set-key (kbd "C-S-n") #'my/consult-org-notes)
(global-set-key (kbd "<f1>") #'my/consult-org-all)
(global-set-key (kbd "<f2>") #'my/consult-org-notes)

;; Full text search the whole org directory
(defun my/consult-ripgrep-org-directory ()
    (interactive)
    (require 'consult)
    ;; Add "--no-ignore-vcs" to the rg command so todo.org could be searched.
    (let ((consult-ripgrep-args (concat consult-ripgrep-args " --no-ignore-vcs")))
        (consult-ripgrep org-directory "")))

(global-set-key (kbd "C-S-f") #'my/consult-ripgrep-org-directory)

;; (defun my/org-breadcrumbs ()
;;   "Get the chain of headings from the top level down
;;     to the current heading."
;;   (let ((breadcrumbs (org-format-outline-path
;;                       (org-get-outline-path t)
;;                       (1- (frame-width))
;;                       nil " > ")))
;;     (format "%s" breadcrumbs)))

;; (defun my/set-header-line-format()
;;   "Set the header line dynamically."
;;   (setq header-line-format '(:eval (my/org-breadcrumbs))))

;; (add-hook 'org-mode-hook #'my/set-header-line-format)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((elisp . t) (shell . t) (python . t) (gnuplot . t) (ditaa . t)))

(setq org-edit-src-content-indentation 0)

;; python3
(setq org-babel-python-command "python3")

;; 2023-10-13: I prefer using M-x org-toggle-pretty-entities instead.
;; (setq org-pretty-entities t)

;; (use-package org-preview-html)

(setq org-clock-mode-line-total 'current) ; default 'auto
(setq org-show-notification-timeout 3600)

(org-clock-auto-clockout-insinuate)

;; Create an indirect buffer for the current clocking task, and focus into it.
(defun my/org-clock-goto ()
    (interactive)
    (org-clock-goto)
    (org-tree-to-indirect-buffer)
    (other-window 1)
    (delete-other-windows))
(global-set-key (kbd "C-c C-x j") 'my/org-clock-goto)

;; Enable the =org-indent-mode= by default.
(setq org-startup-indented t)

;; Reduce org-indent-indentation-per-level from 2 to 1.
;;
;; This keeps =org-tags-column= the same for all headings.
;; Avoid inconsistency when eidting outside Emacs, like Orgzly and Beorg.
(setq org-indent-indentation-per-level 1)

;;;;; fontify
;; 22/10/11--22:18 :: headline 설정 좋다.
(setq org-fontify-todo-headline nil)
;; done 해드라인 폰트 변경을 하지 않는다. 색상 때문에 doom theme 변경시 제대로 안 보임
(setq org-fontify-done-headline nil)
(setq org-fontify-whole-heading-line t)

;; quote 와 verse block 도 배경 색상을 바꾼다
(setq org-fontify-quote-and-verse-blocks t)

;;;;; shift

;; Shift 거슬리는 것을 막아주는 아주 요긴한 설정이다.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-support-shift-select nil) ; default nil
(setq shift-select-mode nil) ; default t

;;;;; imenu ellipsis bookmark

;; Search on https://www.compart.com/en/unicode/U+25BF
;; Unicode Character “◉” (U+25C9)
;; Unicode Character “▾” (U+25BE)
(setq org-imenu-depth 4) ; default 2
(setq org-ellipsis " ◉") ;; "…"
(setq org-capture-bookmark nil)

;;;;; pretty-entities / bullet lists / image-width

(setq org-image-actual-width (min (/ (display-pixel-width) 3) 640))

;; Org styling, hide markup etc. 테스트
;; 왜 minemacs 는 org-pretty 설정을 둘다 t 로 했을까?  org-pretty-entities 가
;; 설정되면 abc_def 에서 def 가 아래로 기어 들어간다.
(setq org-pretty-entities nil) ; very important
;; orgmode 익스포트 할 때, underscore 가 subscripts 변환 방지
;; http://ohyecloudy.com/emacsian/2019/01/12/org-export-with-sub-superscripts/
(setq org-pretty-entities-include-sub-superscripts nil)

;; Replace two consecutive hyphens with the em-dash
(add-hook 'org-mode-hook (lambda ()
                             (push '("---" . "—") prettify-symbols-alist)
                             ;; (push '("->" . "⟶" ) prettify-symbols-alist)
                             ;; (push '("=>" . "⟹") prettify-symbols-alist)
                             (prettify-symbols-mode)))

;; Use utf-8 bullets for bullet lists -- this isn't great, but a bit nicer than nothing.
;; Ideally should use monospace font for spaces before bullet item, and use different bullets by list level.
(font-lock-add-keywords 'org-mode
    '(("^ *\\([+]\\) "
          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; spacemacs style
(add-hook 'org-mode-hook
    (lambda ()
        (setq-local org-emphasis-alist '(("*" bold)
                                            ("/" italic)
                                            ("_" underline)
                                            ("=" org-verbatim verbatim)
                                            ("~" org-kbd)
                                            ("+" (:strike-through t))))))
;; (remove-hook 'org-capture-mode-hook 'spacemacs//org-capture-start) ;; back to default

;;;;; multi-byte

;; 22/10/12--15:49 :: 멀티 바이트 강조
;; https://github.com/clockoon/my-emacs-setting/blob/master/config.org
;; org-mode 는 기본적으로 강조문(굵게, 이탤릭 등)을 하나의 단어에
;; 대해서만 적용하도록 하고 있습니다. 예컨대 *이렇게*는 굵게 글씨를
;; 쓸 수 없습니다. 조사가 들어가는 한중일 언어에 쓰기에는 부적절한
;; 정책입니다. 따라서 강조문자 양 옆에 (알파벳이 아닌) 멀티바이트
;; 문자가 오더라도 작동하도록 설정을 변경합니다(물론 이는 완전한
;; 해결책은 아니며, 더 합리적인 방법에 대해서는 고민이 필요합니다.
(setcar org-emphasis-regexp-components
    " \t('\"{[:multibyte:]")
(setcar (nthcdr 1 org-emphasis-regexp-components)
    "[:multibyte:]- \t.,:!?;'\")}\\")
(org-set-emph-re 'org-emphasis-regexp-components
    org-emphasis-regexp-components)

;; 한자 옆에서도 강조가 되도록
;; (org-set-emph-re 'org-emphasis-regexp-components
;;                  (let ((cjk "[:nonascii:]")) ;; 应该使用 \\cc\\cj\\ch 但 char alternates 不支持 category 所以只能用 char class.
;;                    (pcase-let ((`(,f ,s . ,r) org-emphasis-regexp-components))
;;                      `(,(concat f cjk) ,(concat s cjk) . ,r)
;;                      )
;;                    ))

;;;;; org-startup-folded

;; fold / overview  - collapse everything, show only level 1 headlines
;; content          - show only headlines
;; nofold / showall - expand all headlines except the ones with :archive:
;;                    tag and property drawers
;; showeverything   - same as above but without exceptions
;; #+STARTUP: fold 를 기본값으로 한다. org 파일을 열었을 때, overview 를 가장 먼저 보고 싶기 때문
(setq org-startup-folded 'show2levels)

;;;;; org-src

(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'other-window)

;; DONT org-block and hide leading stars
;; no use for me, I always press this key accidentally
;; (unbind-key "C-'" 'org-mode-map)

;;;;; org-export

;; ;; (setq org-export-preserve-breaks t) ; default nil
;; ;; (setq org-export-with-properties t) ; default nil
;; ;; (setq org-export-with-smart-quotes t) ; default nil
;; ;; (setq org-export-use-babel nil) ; default t
;; ;; (setq org-export-with-broken-links t) ; default nil

(setq org-export-with-todo-keywords t) ; default t
(setq org-export-headline-levels 5) ; default 3
(setq org-publish-use-timestamps-flag t) ; default t
(setq org-export-with-section-numbers nil) ; default t
(setq org-export-with-toc nil) ; default t - turn off on hugo toc

(setq org-export-with-drawers nil) ; default (not "LOGBOOK")

(setq org-export-with-tags 'not-in-toc)

;; Export to MS-Word
;; (setq-default org-odt-preferred-output-format "docx")

;;;;; org-pomodoro

;; A pomodoro group is for a day, so after 8 hours of no activity, that's a group.
(require 'org-pomodoro)
(setq org-pomodoro-expiry-time (* 60 8))
(setq org-pomodoro-manual-break t)
(setq org-pomodoro-play-sounds nil)

(defun ash/org-pomodoro-til-meeting ()
    "Run a pomodoro until the next 30 minute boundary."
    (interactive)
    (let ((org-pomodoro-length (mod (- 30 (cadr (decode-time (current-time)))) 30)))
        (org-pomodoro)))

;;;;; locally-defer-font-lock

(setq jit-lock-defer-time 0)

(defun locally-defer-font-lock ()
    "Set jit-lock defer and stealth, when buffer is over a certain size."
    (when (> (buffer-size) 500000) ; 500kb
        (setq-local jit-lock-defer-time 0.2 ;; 0.05
            jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)

;;;;; visual-line and auto-fill

(add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'spacemacs/toggle-auto-fill-mode-on)
(add-hook 'logos-focus-mode-hook #'spacemacs/toggle-auto-fill-mode-on)

;;;;; org-block and hide leading stars

;; Hide ~*~, ~~~ and ~/~ in org text.
;; org-indent-mode 사용하면 org-hide-leading-stars 자동 on
;; Org styling, hide markup etc. = / ~
(setq org-hide-emphasis-markers t) ; work with org-appear
(setq org-hide-block-startup nil)
(setq org-hide-macro-markers nil)

;; Indentation
;; (if window-system
;;     (setq org-startup-indented t)
;;   (setq org-startup-indented nil))

(setq org-startup-indented nil)
(setq org-hide-leading-stars t)

(setq org-indent-mode-turns-on-hiding-stars nil) ; default t
(setq org-indent-mode-turns-off-org-adapt-indentation t) ; must t, default t

(setq org-indent-indentation-per-level 1) ; default 2

(add-hook 'org-mode-hook #'org-indent-mode)

;;;;; TODO org-columns

;; vedang's style from org-mode-crate
(setq org-columns-default-format
    "%50ITEM(Task) %5Effort(Effort){:} %5CLOCKSUM %3PRIORITY %20DEADLINE %20SCHEDULED %20TIMESTAMP %TODO %CATEGORY(Category) %TAGS")

;;;;; org-agenda-log-mode and clock-mode

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)
(setq org-agenda-start-with-log-mode t)

;; Agenda log mode items to display (closed clock : default)
;; 이전 이맥스는 state 가 기본이었다. 지금은 시간 기준으로 표기한다.
;; closed    Show entries that have been closed on that day.
;; clock     Show entries that have received clocked time on that day.
;; state     Show all logged state changes.
;; (setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-log-mode-add-notes nil)

;; sort 관련 기능을 확인해보고 정의한 함수들이 필요 없으면 빼면 된다.
(setq org-agenda-sort-notime-is-late t) ; Org 9.4
(setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;; Time Clocking
(setq org-clock-idle-time 30) ; 10
(setq org-clock-reminder-timer (run-with-timer
                                   t (* org-clock-idle-time 20) ; 60
                                   (lambda ()
                                       (unless (org-clocking-p)
                                           (alert "Do you forget to clock-in?"
                                               :title "Org Clock")))))
;; (org-clock-auto-clockout-insinuate) ; auto-clockout
;; modeline 에 보이는 org clock 정보가 너무 길어서 줄임
(setq org-clock-string-limit 30) ; default 0
(setq org-clock-history-length 10) ;;
;; org-clock-persist for share with machines
(setq org-clock-persist-query-save t)
(setq org-clock-persist-query-resume t)

;; current  Only the time in the current instance of the clock
;; today    All time clocked into this task today
;; repeat   All time clocked into this task since last repeat
;; all      All time ever recorded for this task
;; auto     Automatically, either all, or repeat for repeating tasks
(setq org-clock-mode-line-entry t)
(setq org-clock-mode-line-line-total 'auto) ; default nil

;;;;; org-tag and category

(setq org-auto-align-tags nil) ; default t
(setq org-tags-column 0) ; default -77
(setq org-agenda-tags-column -80) ;; 'auto ; org-tags-column

(setq org-agenda-show-inherited-tags nil)

;; (setq org-tag-alist (quote ((:startgroup)
;;                             ("@errand" . ?e)
;;                             ("@office" . ?o)
;;                             ("@home" . ?H)
;;                             ("@farm" . ?f)
;;                             (:endgroup)
;;                             ("WAITING" . ?w)
;;                             ("IMPORTANT" . ?i)
;;                             ("NEXT" . ?n)
;;                             ("HOLD" . ?h)
;;                             ("PERSONAL" . ?P)
;;                             ("WORK" . ?W)
;;                             ("FARM" . ?F)
;;                             ("ORG" . ?O)
;;                             ("crypt" . ?E)
;;                             ("savekg" . ?s) ;; save on ekg db
;;                             ("NOTE" . ?N)
;;                             ("CANCELLED" . ?c)
;;                             ("noexport" . ?x)
;;                             ("LATEST" . ?l) ;; latest version
;;                             ("FLAGGED" . ??))))

(add-to-list 'org-tags-exclude-from-inheritance "project")

;;;;; org-agenda-custom-commands

(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'ol-man)

(setq org-agenda-prefix-format
    '((agenda  . " %i %-14:c%?-12t% s")
         (todo  . " %i %-14:c")
         (tags  . " %i %-14:c")
         (search . " %i %-14:c")))

(setq org-agenda-hide-tags-regexp
    "agenda\\|CANCELLED\\|LOG\\|ATTACH\\|GENERAL\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES")

(add-hook 'org-agenda-finalize-hook
    (lambda ()
        ;; (setq-local line-spacing 0.2)
        (define-key org-agenda-mode-map
            [(double-mouse-1)] 'org-agenda-goto-mouse)))

(defun cc/org-agenda-goto-now ()
    "Redo agenda view and move point to current time '← now'"
    (interactive)
    (org-agenda-redo)
    (org-agenda-goto-today)

    (if window-system
        (search-forward "← now ─")
        (search-forward "now -"))
    )

(add-hook 'org-agenda-mode-hook
    (lambda ()
        (define-key org-agenda-mode-map (kbd "<f2>") 'org-save-all-org-buffers)
        (define-key org-agenda-mode-map (kbd "M-p") 'org-pomodoro)
        (define-key org-agenda-mode-map (kbd "M-.") 'cc/org-agenda-goto-now)))


(advice-add 'org-archive :after 'org-save-all-org-buffers)
;; (add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)

;; nil 이면 C-c C-o 으로 접근한다.
;; (setq org-mouse-1-follows-link t) ; default 450

;;;;; element-cache

;; The new org-data element provides properties from top-level property drawer,
;; buffer-global category, and :path property containing file path for file Org buffers.

(setq org-element-use-cache nil) ; default t
;; Element cache persists across Emacs sessions
(setq org-element-cache-persistent nil) ; default t

;;; for crafted emacs : IMPORTANT

(require 'org-agenda)
(require 'evil-org)

(with-eval-after-load 'org-agenda
    (autoload #'evil-org-agenda-set-keys "evil-org-agenda" nil t)
    (evil-org-agenda-set-keys))

;; 이게 없으면 org 파일에서 TAB 문제
(add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
(add-hook 'org-mode-hook 'evil-org-mode)

;;; denote

(require 'denote)
;; (require 'denote-org-extras)

(setq denote-directory org-directory)

(setq denote-modules '(project xref ffap)) ; Enable integration with Emacs modules
(setq denote-prompts '(subdirectory title keywords)) ; These are the minimum viable prompts for notes
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
(define-key global-map (kbd "C-c d") 'denote-map)
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
(global-set-key (kbd "M-g s") 'side-notes-toggle-notes) ;; M-s n

;;; _
(provide 'judy-org)
;;; judy-org.el ends here
