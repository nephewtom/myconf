;; --- Packages ELPA, MELPA, Marmalade ---
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))


;; --- Smart line ---
;; https://github.com/Malabarba/smart-mode-line
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light)
(sml/setup)
(add-to-list 'rm-excluded-modes " MRev")
(add-to-list 'rm-excluded-modes " ARev")

;; --- Move text ---
;; It allows you to move the current line using M-up / M-down
;; if a region is marked, it will move the region instead.
(require 'move-text)
(move-text-default-bindings)

;; --- Git & Svn ---
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

;; --- Company ---
(require 'company)
(setq company-global-modes '(not processing-mode text-mode))
(global-set-key (kbd "M-y") 'company-complete)

(load "~/myconf/emacs/helm-n-buffers.el")
(load "~/myconf/emacs/dired.el")

(load "~/myconf/emacs/nxml.el")
(load "~/myconf/emacs/hideshow.el")
(load "~/myconf/emacs/xah-lookup.el")

(load "~/myconf/emacs/markdown.el")
(load "~/myconf/emacs/org-mode.el")

;;; init-basic-packages.el ends here
