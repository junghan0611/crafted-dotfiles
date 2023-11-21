;;; judy-fonts.el --- Font configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Monoplex KR Nerd" :width 'normal :weight 'regular :height 140)
  (set-fontset-font nil 'hangul (font-spec :family "Monoplex KR Nerd"))
  ;; (set-face-attribute 'fixed-pitch nil :family "Sarasa Term K" :width 'normal :weight 'regular)
  ;; (set-face-attribute 'fixed-pitch-serif nil :family "Hahmlet" :width 'normal :weight 'regular)
  ;; (set-face-attribute 'variable-pitch nil :family "Pretendard Variable"
  ;;                     :width 'normal :weight 'regular)

  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
  (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top

  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil)
  (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend)
  )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@run-hooks (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@run-hooks)

(defun load-theme@theme-dont-propagate (&rest _)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add #'load-theme :before #'load-theme@theme-dont-propagate)

;; ;; (add-hook 'after-load-theme-hook
;; ;;           (defun bolder-faces ()
;; ;;             (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold)
;; ;;             (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold)))

;; (defun +theme--tweaks-h (&optional _)
;;   (interactive)
;;   "Use smaller font (80% of the default) for line numbers in graphic mode."
;;   (when (display-graphic-p)
;;     (set-face-attribute
;;      'line-number nil
;;      :background (face-attribute 'default :background)
;;      :height (truncate (* 0.80 (face-attribute 'default :height)))
;;      :weight 'semi-light)
;;     (set-face-attribute
;;      'line-number-current-line nil
;;      :height (truncate (* 0.80 (face-attribute 'default :height)))
;;      :weight 'bold)))
;; (add-hook 'after-load-theme-hook #'+theme--tweaks-h)

;; ;; 프리셋을 바꿀 경우 필수 수정 요소들
;; (add-hook 'fontaine-set-preset-hook #'+theme--tweaks-h)

;; ;; This is defined in Emacs C code: it belongs to font settings.
;; (setq x-underline-at-descent-line nil) ; conflict with centaur-tabs

;; ;; And this is for Emacs28.
;; (setq-default text-scale-remap-header-line t)

;; ;; | Family                       | Shapes | Spacing | Style      | Ligatures |
;; ;; |------------------------------+--------+---------+------------+-----------|
;; ;; | Sarasa UI K        | Sans   | Compact | Monospaced | Yes       |
;; ;; | Sarasa Mono K      | Sans   | Compact | Monospaced | Yes       |
;; ;; | Sarasa Mono Slab K | Slab   | Compact | Monospaced | Yes       |
;; ;; | Pretendard Variable          | Sans   |         |            | No        |

;; ;; Weights :: Thin ExtraLight Light Regular Medium SemiBold Bold ExtraBold Heavy
;; ;; Slopes :: Upright Oblique Italic
;; ;; Width :: Normal Extended

;; (setq fontaine-presets
;;       ;; 120, 136, 151, 211
;;       '(
;;         (birdview
;;          :default-height 80)
;;         (small
;;          :default-height 120)
;;         (regular
;;          :default-height 136)
;;         (large
;;          :default-height 151)
;;         (presentation
;;          :default-height 180
;;          :line-spacing 3
;;          :fixed-pitch-family "Sarasa Mono Slab K"
;;          :fixed-pitch-serif-family "Sarasa Mono Slab K"
;;          :default-width extended
;;          :bold-weight extrabold)
;;         (presentation-large
;;          :inherit presentation
;;          :default-height 211)
;;         (t
;;          ;; Following Prot’s example, keeping these for for didactic purposes.
;;          :line-spacing 2
;;          :default-family "Sarasa Mono K"

;;          :default-weight regular
;;          :default-height 136
;;          :fixed-pitch-family "Sarasa Mono Slab K"
;;          ;; :fixed-pitch-family "Sarasa Mono K"
;;          :fixed-pitch-weight nil
;;          :fixed-pitch-height nil
;;          ;; :fixed-pitch-serif-family "Sarasa Mono Slab K" ; nil falls back to :default-family
;;          :fixed-pitch-serif-weight nil
;;          :fixed-pitch-serif-height nil
;;          :variable-pitch-family "Pretendard Variable"
;;          ;; :variable-pitch-family "Noto Sans KR"
;;          :variable-pitch-weight nil
;;          :variable-pitch-height nil
;;          :bold-family nil
;;          ;; :bold-weight bold
;;          ;; :bold-width extended
;;          :italic-family nil
;;          :italic-slant italic)))

;; ;; Set last preset or fall back to desired style from `fontaine-presets'.
;; ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
;; ;; (fontaine-set-preset 'regular)
;; ;; (set-fontset-font t 'hangul (font-spec :family (face-attribute 'default :family))) ; t or nil ?

;; ;; store current preset
;; (defun my/fontaine-store-preset ()
;;   (interactive)
;;   (fontaine-store-latest-preset)
;;   ;; (message "my/fontaine-store-preset")
;;   )

;; ;; load @ start-up
;; (defun my/fontaine-load-preset ()
;;   (interactive)
;;   ;; The other side of `fontaine-restore-latest-preset'.
;;   ;; (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)
;;   (fontaine-set-preset 'regular)
;;   ;; ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
;;   (doom-modeline-mode 1)
;;   (modus-themes-toggle) ;; Load Default Themes
;;   )
;; (add-hook 'after-init-hook #'my/fontaine-load-preset 90)

;; (defun my/fontaine-apply-current-preset ()
;;   (interactive)
;;   (fontaine-apply-current-preset)
;;   ;; 한글 사용 위해서 필수!
;;   (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family))) ; default face
;;   (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'variable-pitch :family)) nil 'append) ; for
;;   )

;; ;; POST THEME HOOK
;; (add-hook 'after-load-theme-hook 'my/fontaine-apply-current-preset)

(provide 'judy-fonts)
;;; judy-fonts.el ends here
