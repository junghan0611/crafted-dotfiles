;;; judy-org.el --- org-mode and related module -*- lexical-binding: t; -*-
;;; Commentary:

;;; load org-mode.el

(global-unset-key (kbd "<f6>"))
(global-unset-key (kbd "<f9>"))

;;; user-org-directory

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

(message "`org-directory' has been set to: %s" org-directory)

(setq org-notes-directory (concat org-directory "/roam/notes/"))
(setq org-workflow-directory (concat org-directory "roam/workflow/"))

(setq org-inbox-file (concat org-workflow-directory "20230202T020200--inbox__refile.org"))
(setq org-default-notes-file org-inbox-file)

;; (setq org-user-agenda-files (list (concat org-directory "roam/workflow/")))
(setq org-user-agenda-files (append (file-expand-wildcards (concat org-workflow-directory "*.org"))))
;; (setq org-user-agenda-diary-file (concat org-workflow-directory "20220101T010100--log__agenda.org"))

(setq org-log-file (concat org-workflow-directory "20220101T010100--log__agenda.org"))
(setq org-user-agenda-diary-file org-log-file)


;;; load org-mode.el

(load-file (concat user-emacs-directory "custom-modules/org-mode-a8dff4a.el"))

;;; fix path

;; ;; Set initial buffer to org
(setq initial-major-mode #'org-mode)

;; The following setting is different from the document so that you
;; can override the document org-agenda-files by setting your
;; org-agenda-files in the variable org-user-agenda-files
(if (boundp 'org-user-agenda-files)
    (customize-set-variable 'org-agenda-files (car (list org-user-agenda-files)))
  ;; (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/org/refile.org"))))

;; 2024-01-09 포스트를 여기 추가.
;; org-blog-posts-file
(add-to-list 'org-agenda-files (concat org-notes-directory "20240104T061355--junghanacs-posts.org"))

(if (boundp 'org-user-agenda-diary-file)
    (setq org-agenda-diary-file org-user-agenda-diary-file)
  (setq org-agenda-diary-file "~/org/diary.org"))
(setq diary-file org-agenda-diary-file)

(setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
      org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;; sudo apt-get install ditaa
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

;; (setq org-clock-sound (concat dotspacemacs-directory "assets/sounds/meditation_bell.wav"))
(setq org-crypt-key "B5ADD9F47612A9DB") ; junghanacs

(message "Press `C-c a' to get started with your agenda...")

;;; Code:

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c i") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c \\") 'org-tags-sparse-tree)
(global-set-key (kbd "<f12>") 'org-capture)


;;; paste

;;;;; todo keywords

;; keys mentioned in brackets are hot-keys for the States
;; ! indicates insert timestamp
;; @ indicates insert note
;; / indicates entering the state
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "DONT(o)" "CANCELLED(c@/!)" "PHONE(p)" "MEETING(m)")
              (sequence "NOTE(N)" "KLUDGE(K)" "DEPRECATED(D)" "TEMP(T)" "REVIEW (R)" "FIXED(X)" "FIXME(F)"))))

(defface my/org-bold-todo '((t :inherit (bold org-todo))) "Face for bold TODO-type Org keywords.")
(defface my/org-bold-done '((t :inherit (bold org-done))) "Face for bold DONE-type Org keywords.")
(defface my/org-bold-next '((t :inherit (bold org-todo) :foreground "royal blue" )) "Face for bold NEXT-type Org keywords.")
(defface my/org-bold-shadow '((t :inherit (bold shadow))) "Face for bold and `shadow' Org keywords.")
(defface my/org-bold-note '((t :inherit (bold org-todo) :foreground "DarkOrchid2" )) "Face for bold note-type Org keywords.")
(defface my/org-todo-special '((t :inherit (font-lock-keyword-face bold org-todo))) "Face for special TODO-type Org keywords.")
(setq org-todo-keyword-faces
      '(("TODO" . my/org-bold-todo)

        ("NEXT" . my/org-bold-next)
        ("FIXME" . my/org-bold-next)

        ("DONE" . my/org-bold-done)
        ("FIXED" . my/org-bold-done)
        ("CANCELLED" . my/org-bold-done)

        ("DONT" . my/org-bold-shadow)
        ("WAITING" . my/org-bold-shadow)
        ("HOLD" . my/org-bold-shadow)
        ("DEPRECATED" . my/org-bold-shadow)

        ("NOTE" . my/org-bold-note)
        ("REVIEW" . my/org-bold-note)

        ("MEETING" . my/org-todo-special)
        ("PHONE" . my/org-todo-special)
        ("KLUDGE" . my/org-todo-special)
        ("TEMP" . my/org-todo-special)
        ))

;; (setq org-use-fast-todo-selection t) ; default auto
;; (setq org-use-fast-tag-selection t) ; default auto

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("DEPRECATED" ("noexport" . t))
              ("NOTE" ("LATEST" . t))
              ("REVIEW" ("IMPORTANT" . t))
              ("WAITING" ("WAITING" . t) ("NEXT"))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD") ("NEXT"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD") ("NEXT") ("noexport") ("LATEST") )
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD") ("NEXT" . t) ("noexport") ("LATEST"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD") ("NEXT") ("noexport") ("LATEST"))
              )))

(setq org-priority-faces '((?A . error) (?B . warning) (?C . success)))

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

;; (add-hook 'logos-focus-mode-hook #'spacemacs/toggle-auto-fill-mode-on)

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

;; WATCH vedang's workflow
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
(org-clock-auto-clockout-insinuate) ; auto-clockout
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

(setq org-agenda-sticky nil)

;;;;; org-tag and category

(setq org-auto-align-tags nil) ; default t
(setq org-tags-column 0) ; default -77
(setq org-agenda-tags-column -80) ;; 'auto ; org-tags-column

(setq org-agenda-show-inherited-tags nil)

(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("IMPORTANT" . ?i)
                            ("NEXT" . ?n)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FARM" . ?F)
                            ("ORG" . ?O)
                            ("crypt" . ?E)
                            ("savekg" . ?s) ;; save on ekg db
                            ("NOTE" . ?N)
                            ("CANCELLED" . ?c)
                            ("noexport" . ?x)
                            ("LATEST" . ?l) ;; latest version
                            ("FLAGGED" . ??))))

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
      "agenda\\|LOG\\|ATTACH\\|GENERAL\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES")

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

(setq org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 3)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;;;;; org-capture-templates -- org-refile-file

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-refile-file)
               "* TODO [#C] %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-refile-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-refile-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file org-refile-file)
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file org-refile-file)
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("h" "Phone call" entry (file org-refile-file)
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("H" "Habit" entry (file org-refile-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; ("f" "Fleeting note (/w Clock)" entry (file+headline org-refile-file "Slipbox")
;;   "* TODO %^{Note title}\nContext: %U\n%a\n%?" :clock-in t :clock-resume t)
;; Fleeting Note
;; (push `("f" "Fleeting note" item
;;          (file+headline org-refile-file "Notes")
;;          "+ %U %?" :clock-in t :clock-resume t)
;;   org-capture-templates)

;; One-click Capture for Tasks. Captures the task immediately and gets out of your way.
(push `("T" "Todo Immediate Finish" entry
        (file+headline org-refile-file "FleetBox")
        "* TODO [#C] %^{Todo title}\n%t\n%a\n%?"
        ;; :clock-in t :clock-resume t
        :immediate-finish t)
      org-capture-templates)

;;;;; org-capture-templates -- org-iam-file

;;;;; org-capture-templates -- org-contact-file

(push `("c" "Contacts" entry (file org-contact-file)
        "* %(org-contacts-template-name)
  :PROPERTIES:
  :GITHUB:
  :EMAIL:
  :URL:
  :NOTE:
  :END:\n%U\n%T\n%a\n") org-capture-templates)

;;;;; org-capture-templates -- org-links-file

(push `("l" "links" plain (file+function org-links-file org-capture-goto-link)
        "%i\n%U\n%T\n%a\n" :empty-lines 1 :immediate-finish t)
      org-capture-templates)

;;;;; org-capture-templates -- org-log-file

(setq org-datetree-add-timestamp t)

(push `("j" "Journal" entry (file+olp+datetree org-log-file)
        "* %<%H:%M> - %?\n%U\n" :clock-in t :clock-resume t) ; remove %a
      org-capture-templates)
;; :empty-lines 1 :prepend t -- 역순 등록

;; Capture some feedback for myself or a quick check-in, which I will into other
;; more refined notes later. 나 자신을 위한 피드백이나 간단한 점검 사항을 기록해
;; 두었다가 나중에 좀 더 세련된 노트로 정리할 수 있습니다.
(push `("S" "The Start of Day Planning Routine" entry
        (file+olp+datetree org-log-file)
        (file ,(expand-file-name (concat org-directory "capture-templates/workday.start.org")))
        :prepent t :clock-in t :clock-resume t :empty-lines 1)
      org-capture-templates)

(push `("E" "The End of Day Reflection Routine" entry
        (file+olp+datetree org-log-file)
        (file ,(expand-file-name (concat org-directory "capture-templates/workday.end.org")))
        :prepend nil :clock-in t :clock-resume t :empty-lines 1)
      org-capture-templates)

;;;;; element-cache

;; The new org-data element provides properties from top-level property drawer,
;; buffer-global category, and :path property containing file path for file Org buffers.
(setq org-element-use-cache nil) ; default t

;; Element cache persists across Emacs sessions
(setq org-element-cache-persistent nil) ; default t

;;; for crafted emacs : Important


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
(require 'denote-org-dblock)

(setq denote-directory (concat org-directory "roam/notes/"))

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

;;; org-project-capture

(require 'org-project-capture)
(setq org-project-capture-default-backend (make-instance 'org-project-capture-project-backend))
(setq org-project-capture-projects-file (file-truename (concat org-workflow-directory "20230101T010100--project.org")))
(org-project-capture-single-file)
(push (org-project-capture-project-todo-entry :empty-lines 1)
      org-capture-templates)

;;; _
(provide 'judy-org)
;;; judy-org.el ends here
