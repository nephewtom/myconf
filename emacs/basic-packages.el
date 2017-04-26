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

(load "~/myconf/emacs/helm-n-buffers.el")
(load "~/myconf/emacs/dired.el")

;; --- Move text ---
;; It allows you to move the current line using M-up / M-down
;; if a region is marked, it will move the region instead.
(require 'move-text)
(move-text-default-bindings)

;; --- Company ---
(require 'company)
(setq company-global-modes '(not processing-mode text-mode))
(global-set-key (kbd "M-y") 'company-complete)

;;; init-basic-packages.el ends here
