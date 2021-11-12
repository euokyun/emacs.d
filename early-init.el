;;; early-init.el -*- lexical-binding: t; -*-

;; from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; temporary prevent gc running and reset it later by `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent unwanted runtime compilation for gccemacs.
;; (setq comp-deferred-compilation nil
;;       native-comp-deferred-compilation nil)

(setq native-comp-compiler-options '("-O2" "-march=skylake" "-mtune=native")) ; for gccemacs compile options

;; prevent use stale byte-code.
;; Otherwise skip mtime check on every *.elc file.
(setq load-prefer-newer 'noninteractive)

;; package initialization occurs before `user-init-file' is loaded, but after the `early-init-file'.
(setq package-enable-at-startup nil)


(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

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
(require 'use-package-ensure)
(setq straight-use-package-by-default t)

;; (setq use-package-enable-imenu-support t)

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  ;; :if (memq window-system '(mac ns))    ; while in early-init, this not works. maybe window-system is not recognized.
  ;; :custom
  ;; (exec-path-from-shell-arguments nil)
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


;; Disable most GUI widgets early on
;; (setq default-frame-alist
;;       '((font . "JetBrains Mono")
;;         (horizontal-scroll-bars . nil)
;;         (vertical-scroll-bars . nil)
;;         (ns-appearance . dark)          ; hide titlebar
;;         (ns-transparent-titlebar . t)   ; hide titlebar
;;         (scroll-bar-mode -1)
;;         (tool-bar-mode -1)
;;         (menu-bar-lines . 0)
;;         (tool-bar-lines . 0)
;;         (internal-border-width . 0)
;;         (frame-title-format "\n")
;;         ;; (height . 50)
;;         ;; (width . 95)
;;         (ns-use-proxy-icon nil)
;;         ))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(push '(frame-title-format "\n") default-frame-alist)
(push '(ns-use-proxy-icon nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)          ; hide titlebar
(push '(ns-transparent-titlebar . t) default-frame-alist)   ; hide titlebar
(push '(scroll-bar-mode -1) default-frame-alist)

(setq frame-title-format nil)

(setq frame-inhibit-implied-resize t)


(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(setq buffer-file-coding-system 'utf-8)
(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            (setq file-name-handler-alist default-file-name-handler-alist)
            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)))

;;; early-init.el ends here
