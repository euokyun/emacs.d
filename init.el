;;; init.el --- init.el
;;; Commentary: euokyun's emacs init file.
;;; Code:

(setq debug-on-error t)               ; if you need to debug your init.el
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "*** Emacs loaded in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))


(setq-default
 frame-title-format "\n"                ; hide frame size info that will be second line and not visible.
 ad-redefinition-action 'accept         ; silent warning for redifinition.
 ;; auto-save-default nil                  ; do not make temporary auto-save files. now i use `super-save' instead.
 byte-compile-warnings '(cl-functions)  ; silent old cl-functions warning.
 comp-async-report-warnings-errors nil  ; silent gccemacs native comp warning.
 cursor-in-non-selected-windows nil     ; only shows the cursor of focus buffer.
 cursor-type '(hbar . 2)                ; 2px size horizontal bar
 default-input-method "korean-hangul"   ; input method for korean
 delete-by-moving-to-trash t            ; delete = trash
 delete-selection-mode t                ; override selection
 desktop-save-mode 1                    ; save last frame
 frame-resize-pixelwise t               ; fix frame margin/padding
 mouse-wheel-flip-direction t           ; for OSX -- reverse horizontal scroll.
 mouse-wheel-tilt-scroll t              ; horizontal scroll
 help-window-select t                   ; focus on help window when activated.
 indent-tabs-mode nil                   ; tab=space!
 tab-width 4                            ; space=4!
 inhibit-startup-screen t               ;
 initial-scratch-message nil            ; empty *scratch* buffer.
 keyboard-coding-system 'utf-8-unix     ; utf-8
 large-file-warning-threshold nil       ; do not warn file size.
 make-backup-files nil                  ; do not make temporal backup files.
 mouse-wheel-follow-mouse t             ; scroll buffer under the mouse cursor.
 mouse-wheel-progressive-speed nil      ; scroll speed = wheel speed
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 scroll-step 1                          ; scroll step
 show-help-function nil                 ; t
 straight-use-package-by-default t      ; use use-package
 straight-check-for-modifications '(check-on-save find-when-checking) ; don't catch modification unless `save buffer' command.
 use-package-always-ensure t     ; if package is not installed, then install it.
 tramp-default-method "ssh"      ; remote connection default.
 use-dialog-box nil              ;
 vc-follow-symlinks t            ; silent warning for symlink.
 warning-minimum-level :error    ;
 warning-suppress-log-types '((comp))   ; silent warning for native-comp.
 warning-suppress-types '((use-package) (use-package))
 fill-column 80                         ; default is `70'. force line breaker.
 window-combination-resize t            ;
 x-stretch-cursor t                     ;
 truncate-lines t                       ; do not wrap code.
 backup-by-copying t                    ; make backup.
 ;; global-auto-revert-non-file-buffers t  ; revert dired and other buffers.
 global-auto-revert-mode t
 auto-revert-verbose nil         ; do not message this
 auto-revert-avoid-polling t     ; do not use pooling.
 delete-old-versions t           ; delete old versions.
 version-control t               ;
 scroll-bar-mode 0               ;
 tool-bar-mode 0                 ;
 frame-title-format nil          ; empty titlebar
 ns-use-proxy-icon nil           ; do not use icon in titlebar
 x-underline-at-descent-line t   ; Underline looks a bit better when drawn lower
 inhibit-compacting-font-caches t       ; for fix all-the-icons slow rendering
 face-font-rescale-alist '((".*JetBrains Mono.*" . 1.0)
                           (".*D2Coding.*" . 1.1092896174863387)
                           (".*Iosevka SS08 .*" . 1.2917385677308024)))

(when (eq system-type 'darwin)          ; MacOS specific config
  (setq-default
   ;; ns-use-mwheel-momentum nil
   ns-pop-up-frames nil))

(set-face-attribute 'default nil :height 170) ; initial font size

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use straight.el for use-package expressions
(straight-use-package 'use-package)
;; Load the helper package for commands like `straight-x-clean-unused-repos'
;; (require 'straight-x)
(require 'use-package-ensure)
;; (setq use-package-always-ensure t) ; global ensure



;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; https://github.com/emacscollective/no-littering
;; - var : persistent data
;; - etc : configuration files.
(use-package no-littering
  :init
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  :config
  (require 'recentf) ;; recent files에 var, etc 제외
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))



(fset 'yes-or-no-p 'y-or-n-p)           ; Replace yes/no prompts with y/n
(set-default-coding-systems 'utf-8)
(put 'add-function 'lisp-indent-function 2)
(put 'advice-add 'lisp-indent-function 2)
(put 'plist-put 'lisp-indent-function 2)
;; (mouse-avoidance-mode 'animate)         ; Move the mouse away if the cursor gets close
(winner-mode t)                         ; save window state.
;; Termux 관련 세팅
(setq is-termux
      nil) ; i think i'll not using this on remote
;;       ;; (or (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))))

;; (when is-termux
;;   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(unless is-termux
  ;; (scroll-bar-mode -1)                     ; Disable visible scrollbar
  ;; (tool-bar-mode -1)                       ; Disable the toolbar
  ;; (tooltip-mode -1)                        ; Disable tooltips
  ;; (set-fringe-mode 10)                     ; Give some breathing room
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't)       ; scroll window under mouse
  (setq scroll-step 1)                     ; keyboard scroll one line at a time
  (setq use-dialog-box nil)                ; Disable dialog boxes since they weren't working in Mac OSX
  )

;; fix weird bug with frame size
;; https://stackoverflow.com/questions/27758800/why-does-emacs-leave-a-gap-when-trying-to-maximize-the-frame
;; (setq frame-resize-pixelwise t)

;; 라인 넘버
(column-number-mode)
;; (global-display-line-numbers-mode)

;; 커서 라인 강조
;; (global-hl-line-mode 1)

(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode
            (lambda ()
              (display-line-numbers-mode 1)
              ;; (setq-default
              ;;  ;;  display-line-numbers-type 'relative
              ;;  ;;  display-line-numbers-type t
              ;;  display-line-numbers-grow-only t
              ;;  display-line-numbers-width-start t
              ;;  )
              (hl-line-mode))))


;; 배터리 인디케이터
;; (display-battery-mode)

;; 시계 관련 설정
(setq display-time-world-list
      '(("Asia/Seoul" "Seoul")
        ("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Etc/UTC" "UTC")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")
(setq
 ;; display-time-format "%l:%M %p %b %y"
 display-time-format "%H:%M"
 display-time-default-load-average nil)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               Packages                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; https://github.com/noctuid/general.el
(use-package general
  :init
  (setq general-override-states
        '(insert emacs hybrid normal visual motion operator replace))
  :config
  (general-auto-unbind-keys)
  (general-override-mode)
  (general-evil-setup t))

;; https://github.com/mickeynp/ligature.el
(use-package ligature
  :straight (ligature
             :type git
             :host github
             :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   '(prog-mode racket-repl-mode)
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package diminish)






;;---------------------------
;; ivy
;;---------------------------


(use-package ivy
  :diminish)

;; https://github.com/abo-abo/swiper
(use-package counsel
  :custom
  (ivy-wrap t)
  (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  (counsel-mode))

(use-package smex ;; history 기반 M-x 정렬
  :config
  (smex-initialize))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
            'ivy-switch-buffer
          '(:columns
            ((ivy-rich-candidate (:width 40))
             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)) ; return the buffer indicators
             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning)) ; return the major mode info
             (ivy-rich-switch-buffer-project (:width 15 :face success)) ; return project name using `projectile'
             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))) ; return file path relative to project root or `default-directory' if project is nil
            :predicate
            (lambda (cand)
              (if-let ((buffer (get-buffer cand)))
                  ;; Don't mess with EXWM buffers
                  (with-current-buffer buffer
                    (not (derived-mode-p 'exwm-mode)))))))))

;; https://github.com/lewang/flx
;; https://oremacs.com/2016/01/06/ivy-flx/
(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  ;; :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

;; https://github.com/raxod502/prescient.el
(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; (when (file-directory-p "~/Projects/Code")
  ;;   (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'dw/switch-project-action))

(use-package counsel-projectile
  :after projectile
  ;; :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))



;; ---------------------------
;; Evils
;; ---------------------------

;; read this : https://emacs.stackexchange.com/questions/61512/
;; https://www.dr-qubit.org/Lost_undo-tree_history.html
(use-package undo-tree
  ;; :custom
  ;; (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode t)

  (defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-u-scroll t)
  :custom
  (evil-shift-width tab-width)            ; tab option for evil
  :config
  (evil-mode 1))

;; evil-surround, expend-region and embrace
;; https://github.com/magnars/expand-region.el
(use-package expand-region)

;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package embrace)
;; https://github.com/cute-jumper/evil-embrace.el
(use-package evil-embrace
  :hook ((LaTeX-mode . (lambda () (add-to-list 'evil-embrace-evil-surround-keys ?o))))
  :config
  (evil-embrace-enable-evil-surround-integration))

;; https://github.com/hlissner/evil-multiedit
(use-package evil-multiedit)

;; ;; https://github.com/gabesoft/evil-mc
;; (use-package evil-mc)

(use-package multiple-cursors
  :custom
  (mc/edit-lines-empty-lines 'ignore)
  (mc/insert-numbers-default 1))

(use-package evil-exchange
  :config
  (evil-exchange-install))

;; https://github.com/PythonNut/evil-easymotion
;; (use-package evil-easymotion)

;; https://github.com/abo-abo/avy
(use-package avy
  :custom
  ;; (avy-background t)
  (avy-style 'at-full)
  (avy-timeout-seconds .3)
  ;; :config
  ;; (set-face-italic 'avy-goto-char-timer-face nil)
  ;; (set-face-italic 'avy-lead-face nil)
)

;; https://github.com/emacsorphanage/anzu
;; search and replace feature.
(use-package anzu
  :disabled
  :bind
  ([remap query-replace] . anzu-query-replace-regexp))

;; https://github.com/redguardtoo/evil-nerd-commenter
(use-package evil-nerd-commenter)

;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; https://github.com/cofi/evil-numbers
(use-package evil-numbers)

;; (use-package buffer-move
;;   :config
;;   (setq buffer-move-stay-after-swap t)
;;   (setq buffer-move-behavior 'move))

(use-package evil-collection
  ;; :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

;; ---------------------------
;; Themes Settings
;; ---------------------------

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :config
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
(setq dashboard-set-init-info t)
  (dashboard-setup-startup-hook))

;; https://gitlab.com/jabranham/mixed-pitch
;; match font's pitch
(use-package mixed-pitch
  :disabled
  :hook
  ;; If you want it in all text modes:
  ((text-mode) . mixed-pitch-mode))

;; https://gitlab.com/thomasluquet/font-lock-plus
(use-package font-lock+)


(use-package all-the-icons)

;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-visual-bell-config))


;; https://github.com/purcell/default-text-scale
(use-package default-text-scale)


;; ---------------------------
;; modeline-settings
;; ---------------------------



;; https://github.com/tarsius/minions
(use-package minions
  :custom
  (minions-mode t))


;; https://github.com/seagle0128/doom-modeline
;; (use-package doom-modeline
;;   ;; :after eshell
;;   :hook ((after-init . doom-modeline-mode)
;;          (doom-modeline-mode . minions-mode))
;;   :custom
;;   ;; (doom-modeline-height 15)             ; default is 25
;;   (doom-modeline-height 1)              ; default is 25
;;   (doom-modeline-bar-width 4)
;;   (doom-modeline-project-detection 'auto)
;;   ;; (doom-modeline-buffer-file-name-style 'truncate-except-project)
;;   (doom-modeline-buffer-file-name-style 'truncate-upto-root)
;;   (doom-modeline-icon (display-graphic-p))
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-major-mode-color-icon t)
;;   (doom-modeline-buffer-state-icon t)
;;   ;; (doom-modeline-buffer-state-icon nil)
;;   (doom-modeline-buffer-modification-icon t)
;;   (doom-modeline-unicode-fallback nil)
;;   (doom-modeline-minor-modes t)
;;   (doom-modeline-enable-word-count nil)
;;   (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
;;   (doom-modeline-buffer-encoding t)
;;   (doom-modeline-indent-info nil)
;;   (doom-modeline-checker-simple-format t)
;;   (doom-modeline-number-limit 99)
;;   (doom-modeline-vcs-max-length 12)
;;   (doom-modeline-workspace-name t)
;;   (doom-modeline-persp-name t)
;;   (doom-modeline-display-default-persp-name nil)
;;   (doom-modeline-persp-icon t)
;;   (doom-modeline-lsp t)
;;   (doom-modeline-github nil)
;;   ;; (doom-modeline-github-interval (* 30 60))
;;   (doom-modeline-modal-icon nil)        ; insert, normal, etc...
;;   ;; (doom-modeline-modal-icon t)         ; insert, normal, etc...
;;   ;; (doom-modeline-gnus t)
;;   ;; (doom-modeline-gnus-timer 2)
;;   ;; (doom-modeline-gnus-excluded-groups '("dummy.group"))
;;   (doom-modeline-irc nil)
;;   ;; (doom-modeline-irc-stylize 'identity)
;;   (doom-modeline-env-version t)
;;   (doom-modeline-env-load-string "...")
;;   (doom-modeline-before-update-env-hook nil)
;;   (doom-modeline-after-update-env-hook nil)
;;   (line-number-mode nil)                ; disable line number in modeline.

;;   :custom-face
;;   ;; (mode-line ((t (:height 0.8))))
;;   ;; (mode-line-inactive ((t (:height 0.8))))
;;   ;; (doom-modeline-vspc-face ((t (:height 0.8))))
;;   ;; (doom-modeline-spc-face ((t (:height 0.8))))
;;   ;; global-theme-settings
;;   (doom-modeline-evil-normal-state
;;    ((t (:foreground "orange" :weight bold))))
;;   (doom-modeline-evil-insert-state
;;    ((t (:background "#fb4934" :Weight bold))))

;;   :config
;;   (defun my-doom-modeline--font-height ()
;;     "Calculate the actual char height of the mode-line."
;;     (+ (frame-char-height) 2))
;;   (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

;;   ;; " <N> " is too wide
;;   (doom-modeline-def-segment my/slim-modals
;;     (concat " " (string-trim (doom-modeline-segment--modals))))

;;   (doom-modeline-def-segment my/buffer-info
;;     (concat
;;      (doom-modeline-spc)
;;      (doom-modeline--buffer-mode-icon)
;;      (doom-modeline--buffer-name)
;;      (doom-modeline--buffer-state-icon)))

;;   (doom-modeline-def-modeline 'main
;;     '(bar workspace-name window-number my/slim-modals matches my/buffer-info remote-host buffer-position word-count parrot selection-info)
;;     '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker)))

;; (setq-default mode-line-format nil)
;; (setq mode-line-format nil)









;; https://github.com/tarsius/keycast
;; (use-package keycast
;;   :disabled
;;   :config
;;   (define-minor-mode keycast-mode
;;     "show current command and its key binding in the mode line (fix for use with doom-mode-line)."
;;     :global t
;;     (if keycast-mode
;;         (add-hook 'pre-command-hook 'keycast--update t)
;;       (remove-hook 'pre-command-hook 'keycast--update)))
;;   (add-to-list 'global-mode-string '("" mode-line-keycast)))

;; (use-package awesome-tray
;;   :straight (awesome-tray
;;              :type git
;;              :host github
;;              :repo "manateelazycat/awesome-tray")
;;   )



;; ---------------------------
;; window & buffer
;; ---------------------------

(use-package transpose-frame)

(use-package ace-window
  :init
  ;; (defun my/aw-test-func ()
  ;;   "test function."
  ;;   (interactive)
  ;;   (message "%s" "is?"))
  (defun my/aw-winner-undo ()
    "undo window change."
    (interactive)
    (winner-undo))
  (defun my/aw-winner-redo ()
    "redo window change."
    (interactive)
    (winner-redo))
  (defun my/aw-vterm ()
    "open terminal here"
    (interactive)
    (vterm))
  (defvar aw-dispatch-alist
    '((?c aw-copy-window "copy window")
      (?e aw-execute-command-other-window "execute command other window")
      (?f aw-split-window-fair "split fair window")
      (?j aw-split-window-vert "split vert window")
      (?l aw-split-window-horz "split horz window")
      (?m aw-swap-window "swap windows")
      (?m aw-move-window "move window")
      (?o aw-switch-buffer-in-window "select buffer")
      (?o aw-switch-buffer-other-window "switch buffer other window")
      (?r my/aw-winner-redo)
      (?t aw-transpose-frame "transpose frame")
      (?u my/aw-winner-undo)
      (?x aw-delete-window "delete window")
      (?x delete-other-windows "delete other windows")

      (?! my/aw-vterm)
      ;; ?i ?r ?t are used by hyperbole.el
      (?? aw-show-dispatch-help))
    "list of actions for `aw-dispatch-default'.
each action is a list of either:
  (char function description) where function takes a single window argument
or
  (char function) where function takes no argument and the description is omitted.")
  :custom-face
  (aw-leading-char-face ((t (:height 400 :background "darkorange1" :foreground "#3c3836" :weight bold))))
  ; fixed size.
  :config
  (setq aw-keys '(?a ?s ?d ?k ?g ?h ?b ?v ?q))
  ;; (setq aw-ignore-current t) ; 이러면 split이 현재 윈도우에서 작동하지 않는다.
  (setq aw-minibuffer-flag t)
  (setq aw-dispatch-always t))

;; use same window.
(use-package shackle
  :hook
  (after-init . shackle-mode)
  :custom
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules '((help-mode :same t)
                   (helpful-mode :same t)
                   (process-menu-mode :same t)))
  (shackle-select-reused-windows t))


;;cheatsheet
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  ;; (which-key-setup-minibuffer) ;; paging is not work in minibuffer mode
  (which-key-setup-side-window-right-bottom)
  ;; (setq which-key-use-c-h-commands nil)
  (setq which-key-idle-delay 0.3))


;; ---------------------------
;; indentation
;; ---------------------------

;; https://github.com/joostkremers/visual-fill-column
;; simillar to `visual-line-mode' but not window edge, it uses fill-column to break line.
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  ;; (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  )


;; https://github.com/darthfennec/highlight-indent-guides
(use-package highlight-indent-guides
  :hook ((prog-mode
          ;; org-mode
          )
         . highlight-indent-guides-mode)
  :custom
  ;; (highlight-indent-guides-suppress-auto-error t) ; supress errors.
  (highlight-indent-guides-method 'bitmap)        ; bitmap line only works in gui mode
  ;; (highlight-indent-guides-method 'character)     ; character has bug : sometimes copy character line too.
  ;; (highlight-indent-guides-method 'column)
  (highlight-indent-guides-auto-character-face-perc 25)
  (highlight-indent-guides-delay 0))

;; https://github.com/malabarba/aggressive-indent-mode
;; automatically reindent code
(use-package aggressive-indent
  :disabled
  :hook
  ((css-mode emacs-lisp-mode js-mode lisp-mode sgml-mode) . aggressive-indent-mode)
  :custom
  (aggressive-indent-comments-too t)
  :config
  ;; (add-to-list 'aggressive-indent-protected-commands 'comment-dwim)
  ;; https://github.com/liuchong/aggressive-indent-mode/commit/b59d928060d7aeac55d181711fa7725b695e8bbc
  (setq aggressive-indent-protected-commands
        (append '(undo-tree-visualize
                  undo-tree-visualize-undo
                  undo-tree-visualize-redo
                  evil-undo
                  evil-redo)
                aggressive-indent-protected-commands)))

(use-package origami
  ;; :disabled
  :straight (origami
             :type git
             :host github
             :repo "gregsexton/origami.el"
             :fork (:host github
                          :repo skrytebane/origami.el))
  ;; :hook (yaml-mode . origami-mode)
  :config
  (global-origami-mode))

;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :config
  (ws-butler-mode))


;; ---------------------------
;; File mamager
;; ---------------------------


(use-package all-the-icons-dired)

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :hook ((dired-load-hook . (lambda () (interactive) (dired-collapse)))
         (dired-mode-hook . (lambda ()
                              (interactive)
                              (dired-omit-mode 1)
                              (dired-hide-details-mode 1)
                              (all-the-icons-dired-mode 1)
                              (hl-line-mode 1)))
         (dired-mode-hook . auto-revert-mode))
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))

  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  ;; (add-hook 'dired-load-hook
  ;;           (lambda ()
  ;;             (interactive)
  ;;             (dired-collapse)))

  ;; (add-hook 'dired-mode-hook
  ;;           (lambda ()
  ;;             (interactive)
  ;;             (dired-omit-mode 1)
  ;;             (dired-hide-details-mode 1)
  ;;             (all-the-icons-dired-mode 1)
  ;;             (hl-line-mode 1)))

  ;; (add-hook 'dired-mode-hook 'auto-revert-mode)

  (use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t))

;;               #'(lambda ()
;;                   (if persp-mode-projectile-bridge-mode
;;                       (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
;;                     (persp-mode-projectile-bridge-kill-perspectives))))
;;     (add-hook 'after-init-hook
;;               #'(lambda ()
;;                   (persp-mode-projectile-bridge-mode 1))
;;               t)))

;; ;; https://github.com/nex3/perspective-el
;; (use-package perspective
;;   :demand t
;;   ;; :bind (("C-M-k" . persp-switch)
;;   ;;        ("C-M-n" . persp-next)
;;   ;;        ("C-x k" . persp-kill-buffer*))
;;   :custom
;;   (persp-initial-frame-name "Main")
;;   :config
;;   ;; Running `persp-mode' multiple times resets the perspective list...
;;   (unless (equal persp-mode t)
;;     (persp-mode)))

;; (use-package persp-projectile
;;   :after (perspective projectile))



(use-package treemacs
  :custom
  (imenu-auto-rescan t) ; seems like it's global setting. it will search definitions by name in file.
  (treemacs-tag-follow-mode)
  (treemacs-indent-guide-style 'line)
  :config
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  ;; (use-package treemacs-all-the-icons)
  (general-define-key
   :map treemacs-mode-map
   [mouse-1] #'treemacs-single-click-expand-action ; allow click to expand/collapse node.
   ))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

  (use-package treemacs-evil
    :after (treemacs evil))

;; (use-package treemacs-persp
;;   :after (treemacs persp-mode)
;;   :config (treemacs-set-scope-type 'Perspectives))
;; (use-package treemacs-perspective
;;   :after (treemacs perspective)
;;   :config (treemacs-set-scope-type 'Perspectives))

;; ---------------------------
;; etc
;; ---------------------------

;; https://github.com/purcell/page-break-lines
;; C-q C-c
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package keep-buffers
  :custom
  (keep-buffers-mode 1))

;; https://github.com/Fanael/persistent-scratch
(use-package persistent-scratch
  :custom
  (persistent-scratch-setup-default))

;; https://github.com/Malabarba/spinner.el
(use-package spinner
  :disabled
  :config
  (spinner-start 'moon))

(use-package restart-emacs)

(use-package osx-trash
  :if (memq window-system '(mac))
  :config
  (setq delete-by-moving-to-trash t)
  (osx-trash-setup))

;; https://github.com/xuchunyang/osx-dictionary.el
(use-package osx-dictionary)
;; alternative : https://github.com/abo-abo/define-word

;; https://github.com/raxod502/el-patch
;; for override emacs functions
(use-package el-patch)

;; https://github.com/bbatsov/super-save
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :custom
  (super-save-auto-save-when-idle t)   ; save when idle.
  (auto-save-visited-mode t)
  (super-save-remote-files nil)        ; do not autosave remote files.
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

;; ;; https://github.com/Bad-ptr/persp-mode.el
;; (use-package persp-mode
;;   :hook
;;   (window-setup-hook . (lambda () (persp-mode 1)))
;;   :config
;;   (with-eval-after-load "persp-mode-autoloads"
;;     (setq wg-morph-on nil) ;; switch off animation
;;     (setq persp-autokill-buffer-on-remove 'kill-weak)))

;; (use-package persp-mode-projectile-bridge
;;   :after (persp-mode projectile)
;;   :config
;;   (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
;;     (add-hook 'persp-mode-projectile-bridge-mode-hook

;; https://github.com/Wilfred/helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


;; https://github.com/lastquestion/explain-pause-mode
;; top-like mode. find delay of buffers.
(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :disabled
  :config
  (explain-pause-mode))



;; ---------------------------
;; Git
;; ---------------------------


;; https://magit.vc/manual/magit/
(use-package magit
  ;; :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Major modes for git files.
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :defer t)

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :straight t
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode)
        ;; (git-gutter-mode . (my/git-gutter-theme))
        )
  ;; :hook (text-mode prog-mode)
  ;; These characters are used in terminal mode
  :custom-face
  (git-gutter:added ((t (:background nil))))
  (git-gutter:modified ((t (:background nil))))
  (git-gutter:deleted ((t (:foreground "LightCoral" :background nil))))
  :custom
  (git-gutter:hide-gutter t)
  (git-gutter:modified-sign "≡")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-"))

(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; (persp-switch (projectile-project-name))
  (magit-status))

;; https://elpa.gnu.org/packages/pinentry.html
;; gpg auth
(use-package pinentry
  :disabled
  :hook
  (after-init . pinentry-start))

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-fallback-behavior '(apply tab-jump-out 1))
  (yas-reload-all))

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;; ---------------------------
;; Programming configure
;; ---------------------------


;; https://github.com/flycheck/flycheck
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))
;; (use-package flycheck
;;   :init (global-flycheck-mode))


;; emacs lisp
;; (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

;; https://github.com/emacs-lsp/lsp-mode
;; language server protocol
(use-package lsp-mode
  :straight t
  :defer t
  :commands lsp
  :hook ((java-mode typescript-mode js2-mode web-mode racket-mode) . lsp-deferred)
  :bind (:map lsp-mode-map
              ;; ("TAB" . completion-at-point)
              ("C-c C-f" . lsp-format-buffer))
  :custom
  ;; (lsp-keymap-prefix "C-x l")
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)           ; Use flycheck instead of flymake
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-hook nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-treemacs-sync-mode 1)            ; https://github.com/emacs-lsp/lsp-treemacs
  :config
  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t)))


(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)

 :config (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :straight t
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

;; https://github.com/abo-abo/lispy
;; (use-package lispy
;;   :hook ((emacs-lisp-mode . lispy-mode)
;;          (scheme-mode . lispy-mode)))

;; https://github.com/noctuid/lispyville
(use-package lispyville
  :hook ((emacs-lisp-mode . lispyville-mode)
         (scheme-mode . lispyville-mode))
  ;; :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
                                        additional-movement slurp/barf-cp
                                        prettify commentary))
  (lispyville-enter-visual-when-marking)
  (diminish 'lispyville-mode (lispyville-mode-line-string " 🍰" " 🍰")))

(use-package parinfer
  :disabled
  :bind
  (("C-," . parinfer-toggle-mode))
  :hook
  ((clojure-mode-hook
    emacs-lisp-mode-hook
    common-lisp-mode-hook)
   (scheme-mode-hook)
   (lisp-mode-hook) . parinfer-mode)
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens ; different paren styles for different modes.
             evil          ; If you use Evil.
             lispy ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             ;; paredit        ; Introduce some paredit commands.
             ;; smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))              ; Yank behavior depend on mode.
    ))

;; ---------------------------
;; languages
;; ---------------------------


;; clojure
(use-package cider
  :disabled
  :mode "\\.clj[sc]?\\'"
  :config
  (evil-collection-cider-setup))

;; scheme
;; (use-package scheme-mode
;;   :straight nil
;;   :mode "\\.sld\\'")


;; racket
(use-package racket-mode
  :config (require 'lsp-racket)
  :mode "\\.scm\\'")
(use-package geiser
  :commands (geiser run-geiser geiser-repl)
  :defer t)

;; https://gitlab.com/jaor/geiser/-/issues/224
;; (require 'geiser)
;; (require 'geiser-impl)
(defun cooldown-flycheck-on-racket (&rest _)
   (if (eq geiser-impl--implementation 'racket)
       (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
     (setq flycheck-check-syntax-automatically (default-value 'flycheck-check-syntax-automatically))))

(advice-add 'geiser-set-scheme :after 'cooldown-flycheck-on-racket)
(add-hook 'geiser-mode-hook 'cooldown-flycheck-on-racket)

;; (use-package geiser
 ;;  :init
 ;;  (setq geiser-active-implementations '(guile))


;; Typescript, Javascript
(use-package nvm
  :defer t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

;; Set up proper indentation in JavaScript and JSON files
;; (defun dw/set-js-indentation ()
;;   (setq js-indent-level 2)
;;   (setq evil-shift-width js-indent-level)
;;   (setq-default tab-width 2))

(use-package js-doc)

(use-package js2-mode
  ;; :mode "\\.jsx?\\'"
  :mode (rx ".js" eos)
  :custom
  (js2-mode-show-strict-warnings nil) ; Don't use built-in syntax checking
  (js-switch-indent-offset 2)
  (js2-highlight-level 3)
  (js2-idle-timer-delay 0)
  (js2-mode-show-parse-errors nil)
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  ;; (setq js2-mode-show-strict-warnings nil)
  ;; (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  )

(use-package json-mode
  :mode "\\.json\\'"
  ;; (add-hook 'json-mode-hook #'dw/set-js-indentation)
  )

(use-package rjsx-mode
  :mode
  (rx (or ".jsx"
          (and (or "components" "pages") "/" (* anything) ".js"))
      eos))

;; C/C++
(use-package ccls
  :disabled
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked"))

(use-package web-mode
  ;; :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :mode "(\\.\\(html?\\|ejs\\|tsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  ;; :custom
  ;; (emmet-insert-flash-time 0.1)
  ;; (emmet-move-cursor-between-quote t)
  :hook (web-mode css-mode))

(use-package vue-mode
  :mode "\\.vue\\'"
  :commands (vue-mode))


(use-package yaml-mode
  :mode "\\.ya?ml\\'")




















;; https://www.neilvandyke.org/quack/
;; (use-package quack)

(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind (:map company-active-map
              ;; ("RET" . nil)
              ;; ([return] . nil)
              ([tab] . smarter-tab-to-complete)
              ("TAB" . smarter-tab-to-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0) ;; company-tabnine recommand is 0
  ;; (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  ;; (company-show-quick-access t nil nil "Customized with use-package company")
  (company-show-quick-access t) ;; "Customized with use-package company"
  :config
  ;; (unless clangd-p (delete 'company-clang company-backends))
  (global-company-mode 1)
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.

If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-common))))))

;; https://github.com/TommyX12/company-tabnine
;; this requires run `M-x company-tabnine-install-binary' to install the TabNine binary for your system.
(use-package company-tabnine
  :defer 1
  :custom
  (company-tabnine-max-num-results 9)
  :bind
  ;; (("M-q" . company-other-backend)
  ;;  ("C-z t" . company-tabnine))
  :init
  (defun company//sort-by-tabnine (candidates)
    "Integrate company-tabnine with lsp-mode"
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6)))))
  (defun lsp-after-open-tabnine ()
    "Hook to attach to `lsp-after-open'."
    (setq-local company-tabnine-max-num-results 3)
    (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)))
  (defun company-tabnine-toggle (&optional enable)
    "Enable/Disable TabNine. If ENABLE is non-nil, definitely enable it."
    (interactive)
    (if (or enable (not (memq 'company-tabnine company-backends)))
        (progn
          (add-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
          (add-to-list 'company-backends #'company-tabnine)
          (when (bound-and-true-p lsp-mode) (lsp-after-open-tabnine))
          (message "TabNine enabled."))
      (setq company-backends (delete 'company-tabnine company-backends))
      (setq company-backends (delete '(company-capf :with company-tabnine :separate) company-backends))
      (remove-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
      (company-tabnine-kill-process)
      (message "TabNine disabled.")))
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :config
  (add-to-list 'lsp-client-packages 'lsp-racket)
  (company-tabnine-toggle t))


;; https://github.com/sebastiencs/company-box
(use-package company-box
  :diminish
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook ((company-mode . company-box-mode)
         (company-yasnippet . company-box-mode))
  :custom
  ;; (company-box-doc-enable nil)
  ;; (company-box-backends-colors nil)
  ;; (company-box-doc-delay 0.5)
  (company-box-doc-delay 0)
  (company-box-doc-frame-parameters '((internal-border-width . 1)
                                      (left-fringe . 3)
                                      (right-fringe . 3)))
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

    ;; Credits to Centaur for these configurations
    ;; Display borders and optimize performance
    (defun my-company-box--display (string on-update)
      "Display the completions."
      (company-box--render-buffer string on-update)

      (let ((frame (company-box--get-frame))
            (border-color (face-foreground 'font-lock-comment-face nil t)))
        (unless frame
          (setq frame (company-box--make-frame))
          (company-box--set-frame frame))
        (company-box--compute-frame-position frame)
        (company-box--move-selection t)
        (company-box--update-frame-position frame)
        (unless (frame-visible-p frame)
          (make-frame-visible frame))
        (company-box--update-scrollbar frame t)
        (set-face-background 'internal-border border-color frame)
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border border-color frame)))
      (with-current-buffer (company-box--get-buffer)
        (company-box--maybe-move-number (or company-box--last-start 1))))
    (advice-add #'company-box--display :override #'my-company-box--display)

    (defun my-company-box-doc--make-buffer (object)
      (let* ((buffer-list-update-hook nil)
             (inhibit-modification-hooks t)
             (string (cond ((stringp object) object)
                           ((bufferp object) (with-current-buffer object (buffer-string))))))
        (when (and string (> (length (string-trim string)) 0))
          (with-current-buffer (company-box--get-buffer "doc")
            (erase-buffer)
            (insert (propertize "\n" 'face '(:height 0.5)))
            (insert string)
            (insert (propertize "\n\n" 'face '(:height 0.5)))

            ;; Handle hr lines of markdown
            ;; @see `lsp-ui-doc--handle-hr-lines'
            (with-current-buffer (company-box--get-buffer "doc")
              (let (bolp next before after)
                (goto-char 1)
                (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                  (when (get-text-property next 'markdown-hr)
                    (goto-char next)
                    (setq bolp (bolp)
                          before (char-before))
                    (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                    (setq after (char-after (1+ (point))))
                    (insert
                     (concat
                      (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                      (propertize "\n" 'face '(:height 0.5))
                      (propertize " "
                                  'display '(space :height (1))
                                  'company-box-doc--replace-hr t
                                  'face `(:background ,(face-foreground 'font-lock-comment-face)))
                      (propertize " " 'display '(space :height (1)))
                      (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))

            (setq mode-line-format nil
                  display-line-numbers nil
                  header-line-format nil
                  show-trailing-whitespace nil
                  cursor-in-non-selected-windows nil)
            (current-buffer)))))
    (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

    ;; Display the border and fix the markdown header properties
    (defun my-company-box-doc--show (selection frame)
      (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                (window-configuration-change-hook nil)
                (inhibit-redisplay t)
                (display-buffer-alist nil)
                (buffer-list-update-hook nil))
        (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                       company-box--bottom
                                       company-selection
                                       (company-box--get-frame)
                                       (frame-visible-p (company-box--get-frame))))
                     (candidate (nth selection company-candidates))
                     (doc (or (company-call-backend 'quickhelp-string candidate)
                              (company-box-doc--fetch-doc-buffer candidate)))
                     (doc (company-box-doc--make-buffer doc)))
          (let ((frame (frame-local-getq company-box-doc-frame))
                (border-color (face-foreground 'font-lock-comment-face nil t)))
            (unless (frame-live-p frame)
              (setq frame (company-box-doc--make-frame doc))
              (frame-local-setq company-box-doc-frame frame))
            (set-face-background 'internal-border border-color frame)
            (when (facep 'child-frame-border)
              (set-face-background 'child-frame-border border-color frame))
            (company-box-doc--set-frame-position frame)

            ;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
            (with-current-buffer (company-box--get-buffer "doc")
              (let (next)
                (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                  (when (get-text-property next 'company-box-doc--replace-hr)
                    (put-text-property next (1+ next) 'display
                                       '(space :align-to (- right-fringe 1) :height (1)))
                    (put-text-property (1+ next) (+ next 2) 'display
                                       '(space :align-to right-fringe :height (1)))))))

            (unless (frame-visible-p frame)
              (make-frame-visible frame))))))
    (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

    (defun my-company-box-doc--set-frame-position (frame)
      (-let* ((frame-resize-pixelwise t)

              (box-frame (company-box--get-frame))
              (box-position (frame-position box-frame))
              (box-width (frame-pixel-width box-frame))
              (box-height (frame-pixel-height box-frame))
              (box-border-width (frame-border-width box-frame))

              (window (frame-root-window frame))
              ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                  (/ (frame-pixel-width) 2)
                                                                  (/ (frame-pixel-height) 2)))
              (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

              (x (- (+ (car box-position) box-width) border-width))
              (space-right (- (frame-pixel-width) x))
              (space-left (car box-position))
              (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
              (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
              (width (+ text-width border-width fringe-left fringe-right))
              (x (if (> width space-right)
                     (if (> space-left width)
                         (- space-left width)
                       space-left)
                   x))
              (y (cdr box-position))
              (bottom (+ company-box--bottom (frame-border-width)))
              (height (+ text-height (* 2 border-width)))
              (y (cond ((= x space-left)
                        (if (> (+ y box-height height) bottom)
                            (+ (- y height) border-width)
                          (- (+ y box-height) border-width)))
                       ((> (+ y height) bottom)
                        (- (+ y box-height) height))
                       (t y))))
        (set-frame-position frame (max x 0) (max y 0))
        (set-frame-size frame text-width text-height t)))
    (advice-add #'company-box-doc--set-frame-position
        :override #'my-company-box-doc--set-frame-position))

  (when (require 'all-the-icons nil t)
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page"
                                                :height 1.0
                                                :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width"
                                           :height 1.0
                                           :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube"
                                             :height 1.0
                                             :v-adjust -0.02
                                             :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube"
                                               :height 1.0
                                               :v-adjust -0.02
                                               :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube"
                                                  :height 1.0
                                                  :v-adjust -0.02
                                                  :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag"
                                             :height 1.1
                                             :v-adjust 0
                                             :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag"
                                                :height 1.1
                                                :v-adjust 0
                                                :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component"
                                              :height 1.0
                                              :v-adjust -0.2
                                              :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share"
                                                  :height 1.0
                                                  :v-adjust -0.2
                                                  :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module"
                                               :height 1.0
                                               :v-adjust -0.2
                                               :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench"
                                               :height 1.0
                                               :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream"
                                             :height 1.0
                                             :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right"
                                              :height 1.0
                                              :v-adjust -0.2
                                              :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage"
                                             :height 1.0
                                             :v-adjust -0.2
                                             :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus"
                                                :height 1.0
                                                :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center"
                                                :height 1.0
                                                :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette"
                                              :height 1.0
                                              :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o"
                                           :height 1.0
                                           :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark"
                                                  :height 1.0
                                                  :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open"
                                             :height 1.0
                                             :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right"
                                                   :height 1.0
                                                   :v-adjust -0.2))
            (Constant . ,(all-the-icons-faicon "square-o"
                                               :height 1.0
                                               :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component"
                                               :height 1.0
                                               :v-adjust -0.2
                                               :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap"
                                             :height 1.0
                                             :v-adjust 0
                                             :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point"
                                                 :height 1.0
                                                 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows"
                                                    :height 1.0
                                                    :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left"
                                                 :height 1.0
                                                 :v-adjust -0.2)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

;; https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :after lsp-mode
  :if (executable-find "mvn")
  :init
  (use-package request :defer t)
  :custom
  (lsp-java-server-install-dir (expand-file-name "eclipse.jdt.ls/server/" user-emacs-directory))
  (lsp-java-workspace-dir (expand-file-name "eclipse.jdt.ls/workspace/" user-emacs-directory)))




;; https://github.com/raxod502/apheleia
;; auto code formatter
(use-package apheleia
  :straight (apheleia
             :host github
             :repo "raxod502/apheleia")
  :config
  (apheleia-global-mode +1))

(use-package prettier-js
  :hook (js2-mode typescript-mode)
  :config
  (setq prettier-js-show-errors nil))


;; https://github.com/skeeto/impatient-mode
;; live HTML edit
;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
;; and open `http://localhost:8080/imp/'
(use-package impatient-mode
  :defer t
  :straight t)

;; https://github.com/skeeto/skewer-mode
;; 1. `run-skewer' to attach a brower to emacs
;; 2. from `js2-mode' buffer with `skewer-mode' minor mode enabled,
;;    send forms to the brower to evaluate.
(use-package skewer-mode
  :defer t
  :straight t
  :hook
  ((js2-mode-hook . skewer-mode)
  (css-mode-hook . skewer-css-mode)
  (html-mode-hook . skewer-html-mode)))

;; https://github.com/pashky/restclient.el
;; test HTTP REST webservice.
(use-package restclient
  :mode ((rx ".http" eos) . restclient-mode)
  :bind
  (:map restclient-mode-map
        ([remap restclient-http-send-current]
         . restclient-http-send-current-stay-in-window)
        ("C-n" . restclient-jump-next)
        ("C-p" . restclient-jump-prev))
  :hook
  (restclient-mode . display-line-numbers-mode))

;; (use-package compile
;;   :straight nil
;;   :custom
;;   (compilation-scroll-output t))

;; (defun auto-recompile-buffer ()
;;   (interactive)
;;   (if (member #'recompile after-save-hook)
;;       (remove-hook 'after-save-hook #'recompile t)
;;     (add-hook 'after-save-hook #'recompile nil t)))

;; (use-package smartparens)

;; ;; https://github.com/expez/evil-smartparens
;; (use-package evil-smartparens
;;   :hook smartparens-enabled
;;   :config
;;   (setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
;;   (setq sp-escape-quotes-after-insert nil)
;;   ;; (smartparens-global-strict-mode)
;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;   (when (version<= "27" emacs-version)
;;     (dolist (parens '(c-electric-paren c-electric-brace c-electric-slash))
;;       (add-to-list 'sp--special-self-insert-commands parens))))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; set background color to string's color
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         help-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))


;; https://github.com/victorteokw/tab-jump-out
(use-package tab-jump-out
  :disabled
  :config
  (setq yas-fallback-behavior '(apply tab-jump-out 1))
  (tab-jump-out-mode))


;; ---------------------------
;; Org-mode
;; ---------------------------


;; https://config.daviwil.com/emacs#org-mode
(defun org-mode-setup ()
  (variable-pitch-mode nil)
  (visual-line-mode 1)
  (display-line-numbers-mode nil)
  (setq evil-auto-indent nil))

(use-package org
  :defer t
  :hook ((org-mode . org-mode-setup)
         ;; (org-mode . (lambda ()
         ;;               "Beautify Org Checkbox Symbol"
         ;;               (push '("[ ]" . "☐" ) prettify-symbols-alist)
         ;;               (push '("[X]" . "☑" ) prettify-symbols-alist)
         ;;               (push '("[-]" . "⊡" ) prettify-symbols-alist)
         ;;               (prettify-symbols-mode)))
         (org-mode . prettify-symbols-mode)
         (org-mode . embrace-org-mode-hook)
         ;; (org-mode . turn-on-auto-fill) ; 자동 줄 끊기(auto fill)를 적용한다.
         ;; https://emacs.stackexchange.com/questions/16845/expand-org-mode-subtree-with-point-after-ellipsis/44568
         (org-tab-first-hook . org-end-of-line) ; expand when press TAB after ellipsis.
         )

  :custom
  (org-startup-indented t)              ; Keep the indentation
  (org-src-tab-acts-natively t)         ; indentation setting
  (org-log-done t)       ; Automatically log done times in todo items.
  (org-log-into-drawer t) ; Log stuff into the LOGBOOK drawer by default
  (org-hide-emphasis-markers t)         ; hide markup indicator
  (prettify-symbols-unprettify-at-point 'right-edge)
  (org-fontify-done-headline t)         ; apply special face to DONE
  (org-hide-leading-stars t)            ; hide the stars
  (org-pretty-entities t)               ; show entities as UTF-8 char.
  (org-odd-levels-only)                 ; odd levels only
  (org-indent-indentation-per-level 2)  ;
  (org-hide-leading-stars t)            ; hide the stars.
  (org-ellipsis "⋱")                    ; change ellipsis shape.
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)
  (org-src-tab-acts-natively t)
  ;; (org-edit-src-content-indentation 4)   ; indentation for contents of code block.
  (org-edit-src-content-indentation nil) ; indentation for contents of code block. if `org-src-preserve-indentation' is `non-nil' this will be ignored.
  (org-src-preserve-indentation nil)     ;
  (org-hide-block-startup nil)
  ;; (org-cycle-separator-lines 2)          ; blank line when collapsed. default is `2'
  ;; (org-blank-before-new-entry '((heading . auto) (plain-list-item . auto))) ; default value.
  ;; (org-startup-folded 'content)         ; start with folded content.
  (org-confirm-babel-evaluate nil) ; do not ask confirmation when evaluate code block.
  (prettify-symbols-alist '(("#+BEGIN_SRC" . ?⎡)
                            ("#+END_SRC" . ?⎣)
                            ("#+begin_src" . ?⎡)
                            ("#+end_src" . ?⎣)
                            ;; (">=" . "≥")
                            ("#+begin_quote" . ?«)
                            ("#+end_quote" . ?»)
                            ("#+header:" . ?☰)
                            ;; ("=>" . "⇨")
                            ))

  (org-return-follows-link t)           ; open link with enter key
  (org-refile-targets '((nil :maxlevel . 1)
                        (org-agenda-files :maxlevel . 1)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)

  :custom-face
  (fixed-pitch ((t (:family "JetBrains Mono"))))
  (variable-pitch ((t (:family "D2Coding"))))
  (org-level-1 ((t (:height 1.75))))
  (org-level-2 ((t (:height 1.50))))
  (org-level-3 ((t (:height 1.25))))
  (org-level-4 ((t (:height 1.0))))                 ; ivy-org inherit this.
  (org-level-5 ((t (:height 1.00))))
  (org-level-6 ((t (:height 1.00))))
  (org-level-7 ((t (:height 1.00))))
  (org-level-8 ((t (:height 1.00))))
  ;; (org-ellipsis
  ;;  ((t (:foreground ""))))
  (org-document-title
   ((t (:height 1.1
        :weight bold))))
  ;; (org-done
  ;;  ((t (
  ;;       :foreground "PaleGreen"
  ;;       :strike-through t))))
  ;; (org-headline-done
  ;;  ((t ())))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; [ ] [X]
  (org-checkbox
   ((t (:inherit (fixed-pitch font-lock-keyword-face)))))
  (org-checkbox-done-text
   ((t (:inherit (org-done shadow)))))
  (org-checkbox-statistics-done
   ((t (:inherit (org-done shadow)))))

  ;; (org-indent
  ;;  ((t (:inherit (org-hide fixed-pitch)))))
  (org-code                             ; ~code~
   ((t (:inherit (shadow fixed-pitch)))))
  (org-block
   ((t (;; :foreground nil
        :inherit fixed-pitch))))
  ;; (org-block-begin-line
  ;;  ((t (:inherit fixed-pitch))))
  ;; (org-block-end-line
  ;;  ((t (:inherit fixed-pitch))))
  ;; (org-table
  ;;  ((t (:inherit fixed-pitch))))
  ;; (org-formula
  ;;  ((t (:inherit fixed-pitch))))
  (org-verbatim                         ; =verbatim=
   ((t (:inherit (shadow fixed-pitch)))))
  ;; (org-special-keyword
  ;;  ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-meta-line                        ; #+RESULTS 같은 것들
   ((t (
        :extend t
        :inherit (font-lock-comment-face fixed-pitch)))))
  ;; Get rid of the background on column views
  ;; (org-column
  ;;  ((t (:background nil))))
  ;; (org-column-title
  ;;  ((t (:background nil))))
  ;; (org-date                             ; 날짜
  ;;  ((t ())))




  :config

  ;; (defface org-checkbox-done-text
  ;;  '((t (:inherit (shadow))))
  ;;  "Face for checked checkbox text")


  (doom-themes-org-config)
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)


  (setq org-modules
        '(org-crypt
          org-habit
          org-bookmark
          org-eshell
          ;; org-irc
          ))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (ledger . t)
     ))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  ;; https://github.com/integral-dw/org-superstar-mode
  (use-package org-superstar
    :if (not is-termux)
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-prettify-item-bullets t)
    (org-superstar-item-bullet-alist '((?* . ?•)
                                       ;; (?+ . ?➤)
                                       (?+ . ?‣)
                                       (?- . ?•)))
    (org-superstar-remove-leading-stars t)
    ;; (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))
    ;; (org-superstar-headline-bullets-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶")))
    (org-superstar-headline-bullets-list '("◉" "○" "●" "✜" "●" "✛" "✿" "◆" "▶")))

  ;; Make sure org-indent face is available
  (require 'org-indent)

  ;; (use-package org-indent
  ;;   :ensure nil
  ;;   :straight nil
  ;;   :diminish
  ;;   :custom
  ;;   (org-indent-indentation-per-level 4))



  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  (use-package org-pomodoro
    :after org
    ;; :config
    ;; (setq org-pomodoro-start-sound "~/.dotfiles/.emacs.d/sounds/focus_bell.wav")
    ;; (setq org-pomodoro-short-break-sound "~/.dotfiles/.emacs.d/sounds/three_beeps.wav")
    ;; (setq org-pomodoro-long-break-sound "~/.dotfiles/.emacs.d/sounds/three_beeps.wav")
    ;; (setq org-pomodoro-finished-sound "~/.dotfiles/.emacs.d/sounds/meditation_bell.wav")
    )

  ;; (require 'org-protocol)


  ;; https://github.com/Somelauw/evil-org-mode
  (use-package evil-org
    :after org
    :hook ((org-mode . evil-org-mode)
           (org-agenda-mode . evil-org-mode)
           (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation
                                                                 todo
                                                                 insert
                                                                 textobjects
                                                                 additional)))))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))


  ;; https://github.com/alphapapa/org-make-toc
  (use-package org-make-toc
    :hook (org-mode . org-make-toc-mode))

  (use-package org-appear
    :hook (org-mode . org-appear-mode))

  (use-package org-roam
    :straight t
    :hook
    (after-init . org-roam-mode)
    :config
    (org-roam-db-autosync-mode)
    :custom
    (org-roam-directory "~/Notes/Roam/")
    (org-roam-completion-everywhere t)
    (org-roam-completion-system 'default)
    (org-roam-capture-templates
     '(("d" "default" plain
        #'org-roam-capture--get-point
        "%?"
        :file-name "%<%Y%m%d%H%M%S>-${slug}"
        :head "#+title: ${title}\n"
        :unnarrowed t)
       ("ll" "link note" plain
        #'org-roam-capture--get-point
        "* %^{Link}"
        :file-name "Inbox"
        :olp ("Links")
        :unnarrowed t
        :immediate-finish)
       ("lt" "link task" entry
        #'org-roam-capture--get-point
        "* TODO %^{Link}"
        :file-name "Inbox"
        :olp ("Tasks")
        :unnarrowed t
        :immediate-finish)))
    (org-roam-dailies-directory "Journal/")
    (org-roam-dailies-capture-templates
     '(("d" "default" entry
        #'org-roam-capture--get-point
        "* %?"
        :file-name "Journal/%<%Y-%m-%d>"
        :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
       ("t" "Task" entry
        #'org-roam-capture--get-point
        "* TODO %?\n  %U\n  %a\n  %i"
        :file-name "Journal/%<%Y-%m-%d>"
        :olp ("Tasks")
        :empty-lines 1
        :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
       ("j" "journal" entry
        #'org-roam-capture--get-point
        "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
        :file-name "Journal/%<%Y-%m-%d>"
        :olp ("Log")
        :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
       ("l" "log entry" entry
        #'org-roam-capture--get-point
        "* %<%I:%M %p> - %?"
        :file-name "Journal/%<%Y-%m-%d>"
        :olp ("Log")
        :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")
       ("m" "meeting" entry
        #'org-roam-capture--get-point
        "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
        :file-name "Journal/%<%Y-%m-%d>"
        :olp ("Log")
        :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
    :bind (:map org-roam-mode-map
                (("C-c n l" . org-roam)
                 ("C-c n f" . org-roam-find-file)
                 ("C-c n d" . org-roam-dailies-find-date)
                 ("C-c n c" . org-roam-dailies-capture-today)
                 ("C-c n C r" . org-roam-dailies-capture-tomorrow)
                 ("C-c n t" . org-roam-dailies-find-today)
                 ("C-c n y" . org-roam-dailies-find-yesterday)
                 ("C-c n r" . org-roam-dailies-find-tomorrow)
                 ("C-c n g" . org-roam-graph))
                :map org-mode-map
                (("C-c n i" . org-roam-insert))
                (("C-c n I" . org-roam-insert-immediate))))

  ;; https://github.com/jrblevin/deft
  (use-package deft
    :commands (deft)
    :config (setq deft-directory "~/Notes/Roam"
                  deft-recursive t
                  deft-extensions '("md" "org")))

  ;; (defun org-at-src-block-p ()
  ;;   (eq (nth 0 (org-element-at-point)) 'src-block))

  ;; end org-mode
  )

;; ---------------------------
;; Shell
;; ---------------------------


(use-package vterm
  :defer)

(use-package comint
  :straight nil
  :ensure nil
  :commands (comint-mode shell-command-at-line)
  :bind
  ("C-!" . shell-command-at-line)
  ;; :general
  ;; (:keymaps 'shell-mode-map
  ;;           :states  '(insert emacs)
  ;;           "SPC"    'comint-magic-space)
  :config
  ;; Arrange for Emacs to notice password prompts and turn off echoing for them, as follows:
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)
  (setq ansi-color-for-comint-mode t)   ; package ansi-color
  ;; Auto-kill buffer and window of comint process when done
  (advice-add 'comint-send-eof :after
    (defun comint-kill-after-finish-a (&rest _args)
      (let (confirm-kill-processes kill-buffer-query-functions)
        ;; (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
        (ignore-errors (kill-buffer-and-window)))))

  (defun shell-command-at-line (&optional prefix)
    "Run contents of line around point as a shell command and
replace the line with output. With a prefix argument, append the
output instead."
    (interactive "P")
    (let ( (command (thing-at-point 'line)) )
      (cond ((null prefix)
             (kill-whole-line)
             (indent-according-to-mode))
            (t (newline-and-indent)))
      (shell-command command t nil)
      (exchange-point-and-mark))))
















;; ---------------------------
;; Custom functions
;; ---------------------------


(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

;; override keyboard-escape-quit's delete-other-window function
;; https://stackoverflow.com/questions/557282/in-emacs-whats-the-best-way-for-keyboard-escape-quit-not-destroy-other-windows
(el-patch-defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (current-prefix-arg nil)
        ((and transient-mark-mode mark-active) (deactivate-mark))
        ((> (recursion-depth) 0) (exit-recursive-edit))
        (buffer-quit-function (funcall buffer-quit-function))
        (el-patch-remove ((not (one-window-p t)) (delete-other-windows)))
        ((string-match "^ \\*" (buffer-name (current-buffer)))
         (bury-buffer))))


(defun last-message (&optional num)
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
        (forward-line (- 1 num))
        (backward-char)
        (let ((end (point)))
          (forward-line 0)
          (buffer-substring-no-properties (point) end))))))
(defun insert-last-message (&optional num)
  (interactive "*p")
  (insert (last-message num)))

;; https://emacs.stackexchange.com/a/2198
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))
;; TODO: ace-window와 통합하기


;; (defun un-indent-by-removing-4-spaces ()
;;   "Remove 4 spaces from beginning of of line."
;;   (interactive)
;;   (save-excursion
;;     (save-match-data
;;       (beginning-of-line)
;;       ;; get rid of tabs at beginning of line
;;       (when (looking-at "^\\s-+")
;;         (untabify (match-beginning 0) (match-end 0)))
;;       (when (looking-at "^    ")
;;         (replace-match "")))))

;; fix non-compatiable command for evil.
(defun my/move-forward-char () (forward-char 1))
(advice-add 'org-ctrl-c-ret :before #'my/move-forward-char)
(advice-add 'lispy-eval-and-comment :before #'my/move-forward-char)
(advice-add 'racket-send-last-sexp :before #'my/move-forward-char)
(advice-add 'racket-send-definition :before #'my/move-forward-char)
;; (advice-add 'eval-last-sexp :before #'(forward-char 1))

;; org-mode functions
(defun dw/search-org-files ()
  "Find Org files in ~/Notes."
  (interactive)
  (counsel-rg "" "~/Notes" nil "Search Notes: "))

;; tail `*Messages*' buffer
(advice-add 'message :after
  (defun me/message-tail (&rest _)
    (let* ((name "*Messages*")
           (buffer (get-buffer-create name)))
      (when (not (string= name (buffer-name)))
        (dolist (window (get-buffer-window-list name nil t))
          (with-selected-window window
            (goto-char (point-max))))))))








;; ---------------------------
;; Keybinding
;; ---------------------------

(general-unbind 'global
  "s-p"
  "s-h"
  )



(general-define-key
 "C-s" 'counsel-grep-or-swiper
 ;; "s-f" 'counsel-grep-or-swiper
 ;; "s-w" 'delete-window
 ;; "s-W" 'delete-other-window
 ;; undo, redo
 ;; "s-z" 'undo-tree-undo
 ;; "s-Z" 'undo-tree-redo
 "C-M-u" 'universal-argument            ; C-u is now evil-scroll-up
 "s-?" 'evilnc-copy-and-comment-lines
 "s-/" 'evilnc-comment-or-uncomment-lines
 "<f17>" 'toggle-input-method
 "M-y" 'insert-last-message
 "s-b" 'treemacs
 "C-SPC" 'completion-at-point
 "s-o" 'find-file
 "s-f" 'evil-avy-goto-char-timer

 )

(general-define-key
 :states '(normal insert)
 :keymaps 'repl-mode
 "C-n")


(unless is-termux
  (general-unbind '(normal motion)
    "<left>"
    "<right>"
    "<down>"
    "<up>"))

(general-define-key
 :states 'insert
 "s-d" 'evil-multiedit-toggle-marker-here)

(general-define-key
 :states 'visual
 "R" 'evil-multiedit-match-all
 "C-M-d" 'evil-multiedit-restore)

(general-define-key
 :states '(normal visual)
 "s-d" 'evil-multiedit-match-and-next
 "s-D" 'evil-multiedit-match-and-prev)

(push '((multiedit-insert . evil-multiedit-insert-state-map)
        (multiedit . evil-multiedit-state-map)) general-keymap-aliases)
(general-define-key
 :states '(multiedit motion)
 "RET" 'evil-multiedit-toggle-or-restrict-region)

(general-define-key
 :states '(multiedit multiedit-insert)
 "C-n" 'evil-multiedit-next
 "C-p" 'evil-multiedit-prev)






;; ESCAPERS--
(general-define-key
 :states '(motion)
 :keymaps '(undo-tree-visualizer-mode-map)
 "<escape>" 'undo-tree-visualizer-quit)

(general-define-key
 :states '(normal)
 :keymaps '(help-mode-map helpful-mode-map)
 "<escape>" 'quit-window)

(general-define-key
 :states '(normal)
 :keymaps 'debugger-mode-map
 "<escape>" 'top-level)
;; --ESCAPERS
;; (general-define-key
;;  ;; :keymaps '(help-mode-map helpful-mode-map backtrace-mode-map custom-mode-map diff-minor-mode-map )
;;   :keymaps 'override
;;  "SPC" nil)

(general-create-definer spc-leader
  :keymaps 'override
  ;; :keymaps '(normal insert visual emacs motion)
  ;; :global-prefix "C-SPC"
  :global-prefix "M-SPC"
  :prefix "SPC")


(spc-leader
  :keymaps '(normal insert visual emacs motion)

  ;; "" nil
  "u" '(undo-tree-visualize :which-key "undo-tree")
  "<left>" 'winner-undo
  "<right>" 'winner-redo
  "w" '(ace-window :which-key "ace-window")

  ;; magit settings
  "g"   '(:ignore t :which-key "magit")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase

  ;; ;; non-normal-states 기능
  ;; "m" '(:keymap evilem-map :which-key "easymotion")
  "m" 'evil-avy-goto-line

  ;; projectile
  "p" '(:ignore t :which-key "projectile")
  "pf" 'counsel-projectile-find-file
  "ps" 'counsel-projectile-switch-project
  "p s-f" 'counsel-projectile-rg
  ;; "pF"  'consult-ripgrep
  "pp" 'counsel-projectile
  "pc" 'projectile-compile-project
  "pd" 'projectile-dired

  ;; expand region
  "v" 'er/expand-region
  )
;; lsp-mode
(spc-leader
  :states '(normal insert visual emacs motion)
  :keymaps 'lsp-mode-map
  "l" '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)


;; org-mode
(spc-leader
  :states '(normal insert visual emacs motion)
  :keymaps 'org-mode-map
  "o" '(:ignore t :which-key "org-mode")
  "op" '(org-pomodoro :which-key "pomodoro")
  "oi" '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")
  "on" '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
  "of" '(dw/search-org-files :which-key "search notes")
  ;; "os"  '(counsel-rg :which-key "search notes")
  "os" '(org-agenda :which-key "status")
  "ot" '(org-todo-list :which-key "todos")
  "oc" '(org-capture t :which-key "capture")
  "ox" '(org-export-dispatch t :which-key "export"))













(general-create-definer spc-e
  :prefix "SPC e")

(spc-e
  :states 'normal
  ;; :which-key "eval"
  ;; :keymaps
  ;; "" '(:ignore t :which-key "eval")
  "b" '(eval-buffer :which-key "eval buffer")
  "/" '(lispy-eval-and-comment :which-key "eval and comment")
  "x" '(eval-last-sexp :which-key "eval sexp"))

(spc-e
  :keymaps '(visual)
  ;; :which-key "eval"
  "r" '(eval-region :which-key "eval region"))

(spc-e
  ;; :keymaps '(scheme-mode-map inferior-scheme-mode-map)
  :keymaps '(racket-mode-map)
  :states 'normal
  ;; "p" 'racket-repl
  "b" 'racket-run-module-at-point
  ;; "x" 'racket-send-last-sexp-evil-fix
  ;; "/" 'lispy-eval-and-comment-evil-fix
  ;; "d" 'racket-send-definition-evil-fix
  "x" 'racket-send-last-sexp
  ;; "/" 'lispy-eval-and-comment
  "d" 'racket-send-definition)

(spc-e
  ;; :keymaps '(scheme-mode-map inferior-scheme-mode-map)
  :keymaps '(racket-mode-map)
  :states 'visual
  "r" 'racket-send-region)

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "h" 'dired-single-up-directory
 "H" 'dired-omit-mode
 "l" 'dired-single-buffer
 "y" 'dired-ranger-copy
 "X" 'dired-ranger-move
 "p" 'dired-ranger-paste)

(general-define-key
 :keymap 'lispyville-mode-map
 "s-/" 'lispyville-comment-or-uncomment)



;; ;; org-mode
(general-define-key
 :states '(normal insert visual)
 :keymaps 'org-mode-map
 "C-j" 'org-next-visible-heading
 "C-k" 'org-previous-visible-heading)




;; apply theme
;; ns-system-appearance
;; => dark
(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-gruvbox-light t))
    ('dark (load-theme 'doom-gruvbox t)))
  (when (facep 'git-gutter:modified)
    (pcase appearance
      ('light (face-remap-add-relative 'git-gutter:modified nil '(:foreground "Black")))
      ('dark (face-remap-add-relative 'git-gutter:modified nil '(:foreground "LightGoldenrod")))))
  (when (facep 'git-gutter:added)
    (pcase appearance
      ('light (face-remap-add-relative 'git-gutter:added nil '(:foreground "DarkGreen")))
      ('dark (face-remap-add-relative 'git-gutter:added nil '(:foreground "LightGreen"))))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
(my/apply-theme ns-system-appearance)

;; (defun my/git-gutter-theme (appearance)
;;   "This is theme setting for git-guttter, for MacOS's light and dark theme."
;;   (let ((appearance ns-system-appearance))
;;     (when (facep 'git-gutter:modified)
;;       (pcase appearance
;;         ('light (face-remap-add-relative 'git-gutter:modified nil '(:foreground "Black")))
;;         ('dark (face-remap-add-relative 'git-gutter:modified nil '(:foreground "LightGoldenrod")))))
;;     (when (facep 'git-gutter:added)
;;       (pcase appearance
;;         ('light (face-remap-add-relative 'git-gutter:added nil '(:foreground "DarkGreen")))
;;         ('dark (face-remap-add-relative 'git-gutter:added nil '(:foreground "LightGreen")))))))


(use-package gcmh
  :init
  (gcmh-mode 1))

(provide 'init)
;;; init.el ends here
