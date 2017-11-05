;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

;; From
;; https://realpython.com/blog/python/emacs-the-best-python-editor/

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    material-theme
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally







;; init.el ends here
;; --- Python... this is incomplete
(elpy-enable)
(defun my-python-hook ()
  "This is my python hook function."
  (elpy-use-ipython)
  )
(add-hook 'python-mode-hook 'my-python-hook)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  "Disable AC (auto-complete mode) for python mode, as elpy use company."
  (unless (eq major-mode 'python-mode) ad-do-it))
(ad-activate 'auto-complete-mode)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
