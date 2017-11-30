;;; init.el starts here

;; Try to speed up start-up
(setq-default gc-cons-threshold (* 100 1024 1024))
(let ((file-name-handler-alist nil)) "~/myconf/emacs/init.el")

;; Follow git symlinks
(setq vc-follow-symlinks t)

;; This comes first
(if (not (boundp 'basic-conf))
    (load "~/myconf/emacs/basic.el"))


;; --- Packages ELPA, MELPA, Marmalade ---
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))

;;(load "~/myconf/emacs/smart-line.el")
(load "~/myconf/emacs/old/xah-lookup.el")
(load "~/myconf/emacs/old/edit-with-emacs.el")
(load "~/myconf/emacs/old/nxml.el")
(load "~/myconf/emacs/old/hideshow.el")
(load "~/myconf/emacs/old/markdown.el")
(load "~/myconf/emacs/old/helm.el")
(load "~/myconf/emacs/old/magit.el")
(load "~/myconf/emacs/old/python.el")
(load "~/myconf/emacs/old/org-mode.el")

(setq custom-file "~/myconf/emacs/custom.el")
(load custom-file)
(put 'scroll-left 'disabled nil)

;;(server-start) ;; emacs server
(message "Emacs ready with old-init.el !")
;;; old-init.el ends here
