;;; early-init.el -*- lexical-binding: t; -*-

;; from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; temporary prevent gc running and reset it later by `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum
      ;; gc-cons-percentage 0.6
      )

;; Prevent unwanted runtime compilation for gccemacs.
;; (setq comp-deferred-compilation nil
;;       native-comp-deferred-compilation nil)

(setq native-comp-compiler-options '("-O2" "-march=skylake" "-mtune=native")) ; for gccemacs compile options

(setq max-specpdl-size 13000)

;; prevent use stale byte-code.
;; Otherwise skip mtime check on every *.elc file.
(setq load-prefer-newer 'noninteractive)

;; package initialization occurs before `user-init-file' is loaded, but after the `early-init-file'.
(setq package-enable-at-startup nil)

;; ----------------------------------------------------------------
;; use straight.el as package manager.
;; will cache autoloads for all package. I/O workload will be O(n) -> O(1).
;; you may wish to call `straight-prune-build' occasionally, since otherwise this cache file may grow quite large over time.
(setq straight-cache-autoloads t)

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
;; (require 'use-package-ensure)
(setq straight-use-package-by-default t
      straight-check-for-modifications '(check-on-save find-when-checking))

(advice-add 'straight-prune-build
    :before #'(lambda ()
                (delete-file
                 (expand-file-name
                  ".DS_Store"
                  (expand-file-name
                   straight-build-dir
                   (concat straight-base-dir "straight"))))))


;; (straight-use-package 'esup)

;; (setq use-package-enable-imenu-support t)

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments 'nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH" "SHELL" "JAVA_HOME"))
  :config
  (exec-path-from-shell-initialize))

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

;; frame
(setq frame-title-format nil
      frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0)             default-frame-alist)
(push '(tool-bar-lines . 0)             default-frame-alist)
(push '(internal-border-width . 0)      default-frame-alist)
(push '(frame-title-format "\n")        default-frame-alist)
(push '(ns-use-proxy-icon nil)          default-frame-alist)
(push '(horizontal-scroll-bars . nil)   default-frame-alist)
(push '(vertical-scroll-bars . nil)     default-frame-alist)
(push '(ns-appearance . dark)           default-frame-alist) ; hide titlebar
(push '(ns-transparent-titlebar . t)    default-frame-alist) ; hide titlebar
;; (push '(scroll-bar-mode -1)             default-frame-alist)

;; (add-to-list 'default-frame-alist '(undecorated . t))

(setq scroll-bar-mode nil
      tool-bar-mode nil)

;; Fundamental mode at startup
(setq initial-major-mode 'fundamental-mode)

(defun display-startup-echo-area-message ()
  (message ""))

(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(setq buffer-file-coding-system 'utf-8)

;;; early-init.el ends here
