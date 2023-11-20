;;; meow-config.el --- Configuring Emacs -*- lexical-binding: t; -*-

;;;; TODO Move
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(progn
  (global-set-key (kbd "C-s-h") 'tab-previous)
  (global-set-key (kbd "C-<backtab>") 'tab-previous)
  (global-set-key (kbd "C-s-l") 'tab-next)
  (global-set-key (kbd "s-\\") 'tab-bar-switch-to-tab)
  ;; (global-set-key (kbd "C-s-1") #'(lambda() (interactive) (tab-bar-select-tab 1)))
  ;; (global-set-key (kbd "C-s-2") #'(lambda() (interactive) (tab-bar-select-tab 2)))
  ;; (global-set-key (kbd "C-s-3") #'(lambda() (interactive) (tab-bar-select-tab 3)))
  ;; (global-set-key (kbd "C-s-4") #'(lambda() (interactive) (tab-bar-select-tab 4)))
  ;; (global-set-key (kbd "C-s-5") #'(lambda() (interactive) (tab-bar-select-tab 5)))
  ;; (global-set-key (kbd "C-s-6") #'(lambda() (interactive) (tab-bar-select-tab 6)))
  ;; (global-set-key (kbd "C-s-7") #'(lambda() (interactive) (tab-bar-select-tab 7)))
  )

;; If you use a window manager be careful of possible key binding clashes
(global-set-key (kbd "M-<tab>") 'other-window) ; very useful
(global-set-key (kbd "M-<iso-lefttab>") (lambda() (interactive) (other-window -1))) ; == M-S-<tab>
(global-set-key (kbd "M-<backtab>") (lambda() (interactive) (other-window -1))) ; for terminal

;;;; Which Key
(require 'which-key)

(setq which-key-idle-delay 0.4
      which-key-min-display-lines 3
      which-key-idle-secondary-delay 0.01
      which-key-max-description-length 32
      which-key-sort-order 'which-key-key-order-alpha
      ;; which-key-allow-evil-operators t
      )

;; from lambda-emacs
;; Allow C-h to trigger which-key before it is done automatically
;; (setq which-key-show-early-on-C-h t)
;; Set the time delay (in seconds) for the which-key popup to appear.
;; make sure which-key doesn't show normally but refreshes quickly after it is
;; triggered.
;; (setq which-key-idle-delay .75)
;; (setq which-key-idle-secondary-delay 0.05)
;; (setq which-key-popup-type 'side-window)
;; (setq which-key-side-window-max-height 0.5)
;; (setq which-key-allow-imprecise-window-fit nil)
;; (setq which-key-side-window-location 'top)

(which-key-mode)

;;; meow-setup

;; /home/junghan/sync/man/dotsamples/vanilla/mememacs-meow-clojure-copy/lisp/init-meow.el:1
;; /home/junghan/lambda-emacs/lambda-library/lambda-user-samples/cpm-setup-meow.el:216

(require 'meow)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("h" . meow-left)
   '("," . meow-keypad)
   '("/" . isearch-forward)
   '("<escape>" . ignore))

  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   ;; '("j" . "H-j")
   ;; '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("bb" . consult-buffer)
   '("bB" . spacemacs/compleseus-switch-to-buffer)
   '("bd" . spacemacs/kill-this-buffer)
   '("bx" . kill-buffer-and-window)
   '("bm" . switch-to-messages-buffer)
   '("bh" . meow-last-buffer)
   '("ss" . consult-line)
   '("sS" . consult-line-symbol)
   '("sg" . consult-grep)
   '("sd" . my/compleseus-search-dir)
   '("sD" . spacemacs/compleseus-search-dir)
   '("sf" . spacemacs/compleseus-search-auto)
   '("sF" . my/compleseus-search-auto-hidden)
   '("ff" . spacemacs/compleseus-find-file)
   '("fs" . save-buffer)
   '("fD" . spacemacs/delete-current-buffer-file)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . hydra-denote/body)
   '("'" . repeat)
   '("<escape>" . ignore)))

(meow-global-mode 1)
(meow-setup)

(with-eval-after-load 'meow
  (defvar +input-method-state nil)

  (add-hook 'meow-insert-mode-hook
            (lambda ()
              (when +input-method-state
                (activate-input-method +input-method-state)))) ; 입력 모드로 가면 한글 모드 였다면 다시 활성화.

  (defadvice meow-insert-exit (after ad-meow activate)
    (setq +input-method-state current-input-method) ;; 저장
    (when current-input-method
      (deactivate-input-method))) ;; 무조건 영어로 변경. 잇풋 메소드 끈다.

  (defadvice activate-input-method (after ad-meow activate)
    (when (meow-normal-mode-p)
      (when current-input-method
        ;; 이건 다른 방식으로 처리 필요. 모드 별 키바인딩
        (message "Input method is disabled in normal state.")
        ;; (meow-normal-mode)
        ))
    )

  ;; 한글 끌 때는 상태 정보도 지워야 한다.
  (add-hook 'input-method-deactivate-hook
            (lambda ()
              (when current-input-method
                (setq +input-method-state nil)
                )))
  )

;;;  provide

(provide 'meow-config)

;;;  ends here
