;;; early-init.el -*- lexical-binding: t; -*-

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

(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (menu-bar-mode 1)

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            (setq file-name-handler-alist default-file-name-handler-alist)
            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 170)

(provide 'early-init)
;;; early-init.el ends here
