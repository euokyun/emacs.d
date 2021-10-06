;;; -*- lexical-binding: t -*-

;; from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; temporary prevent gc running and reset it later by `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent unwanted runtime compilation for gccemacs.
;; (setq comp-deferred-compilation nil
;;       native-comp-deferred-compilation nil)


;; prevent use stale byte-code.
;; Otherwise skip mtime check on every *.elc file.
(setq load-prefer-newer noninteractive)

;; package initialization occurs before `user-init-file' is loaded, but after the `early-init-file'.
(setq package-enable-at-startup nil)

;; (member "D2Coding" (font-family-list))
;; (member "JetBrains Mono" (font-family-list))
;; (member "Iosevka SS08" (font-family-list))


(set-face-attribute 'default nil :family "JetBrains Mono")
(set-fontset-font t 'hangul "D2Coding")

;; Use 'prepend for the NS and Mac ports or Emacs will crash.
(set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
(set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
(set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
(set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
(set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
(set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)


(scroll-bar-mode 0)
(tool-bar-mode 0)

(setq default-frame-alist
      '(
        ;; (scroll-bar-mode . 0)
        ;; (tool-bar-mode . 0)
        (font . "JetBrains Mono")))




(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            (setq file-name-handler-alist default-file-name-handler-alist)
            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)))

;;; early-init.el ends here
