;; Provide timestamp to *Messages* logs
(load "~/myconf/emacs/log.el")

;; Try to speed up start-up
(setq-default gc-cons-threshold (* 100 1024 1024))

(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(require 'use-package)

(load "~/myconf/emacs/general.el")
(load "~/myconf/emacs/column-and-line-numbers.el")
(load "~/myconf/emacs/paren-indent.el")
(load "~/myconf/emacs/cond-mac-linux-win.el")
(load "~/myconf/emacs/duplicate-line.el")
(load "~/myconf/emacs/xah-cut-copy.el")
(load "~/myconf/emacs/compilation.el")
(load "~/myconf/emacs/defalias.el")

(load "~/myconf/emacs/dired.el")
(load "~/myconf/emacs/ediff.el")
(load "~/myconf/emacs/movement.el")
(load "~/myconf/emacs/buffers-utils.el")
(load "~/myconf/emacs/elisp.el")
(load "~/myconf/emacs/company.el")
;;(load "~/myconf/emacs/xah-lookup.el")

(load "~/myconf/emacs/keybindings.el")

(defvar basic-conf t
  "t if my personal basic Elisp configuration was loaded")

;; Problem when hitting Alt+Tab on Windows
(global-set-key (kbd "<Scroll_Lock>") 'ignore)

(message "Emacs ready with basic.el !")
;;; basic.el ends here

