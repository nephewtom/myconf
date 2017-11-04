;; TODO: Check /use-package/ module...

;; TODO: Autoload preferred

;; TODO: Check startup profilers:
;; https://oremacs.com/2015/02/24/emacs-speed-test/
;; https://github.com/jschaf/esup
;; https://github.com/dholm/benchmark-init-el

;;; It allows you to move the current line using M-up / M-down
;; If a region is marked, it will move the region instead.
(require 'move-text)
(move-text-default-bindings)

(load "~/myconf/emacs/smart-line.el")

;; not working if only loading this basic.el file
;; probably need the require package & refresh
(load "~/myconf/emacs/company.el")
(company-mode)


(require 'package)
;; --- Packages ELPA, MELPA, Marmalade ---
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))


