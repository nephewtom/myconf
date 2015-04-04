(server-start)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(cua-mode t)
(column-number-mode t)
(show-paren-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)

(global-linum-mode t)
(global-hl-line-mode t)
(set-face-background hl-line-face "#FBFFAB")
(set-face-attribute 'default nil :height 140)

(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame
(global-set-key [C-tab] 'other-window)


;; --- Packages ELPA, MELPA, Marmalade ---
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(set-face-attribute 'fringe nil :background "#d0f0ff")

;; --- Buffers & Ibuffer stuff ---
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

(defun switch-to-previous-buffer ()
  "Swap to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "<f8>") 'switch-to-previous-buffer)


;; --- Company Testing...

(global-company-mode)
(setq company-global-modes '(not emacs-lisp-mode processing-mode))
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)
(global-set-key (kbd "M-y") 'company-complete)
