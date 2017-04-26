;;; package --- Emacs init-basic-packages.el file
;;; Commentary:

;;; Code:

(setq package-enable-at-startup nil)
(package-initialize)

(scroll-bar-mode t)

;; --- Packages ELPA, MELPA, Marmalade ---
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))


;; --- Smart line ---
;; https://github.com/Malabarba/smart-mode-line
(setq sml/no-confirm-load-theme t)
(sml/setup)
(add-to-list 'rm-excluded-modes " MRev")
(add-to-list 'rm-excluded-modes " ARev")


;; --- Helm ---
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent actionn

;; --- Move text ---
;; It allows you to move the current line using M-up / M-down
;; if a region is marked, it will move the region instead.
(require 'move-text)
(move-text-default-bindings)


;; --- Buffers & Ibuffer stuff ---
;; Remove from Ibuffers the buffers that match these regexp
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*helm ag")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm mini")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm find")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm grep exts")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm M-x")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm-mode")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm buffers")
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
(add-to-list 'ibuffer-never-show-predicates "^\\*Disabled")
(add-to-list 'ibuffer-never-show-predicates "^\\*Help")
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp")
(add-to-list 'ibuffer-never-show-predicates "^\\*JDEE")
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1))) ;; Update ibuffer automatically


;; --- Dired ---
(require 'dired )
(setq dired-listing-switches "-lk")
;; move to up directory with '.'
(define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
;; Following key is already binded to 'a'
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq dired-auto-revert-buffer t)


;;; init-basic-packages.el ends here
