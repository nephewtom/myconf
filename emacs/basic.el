(load "~/myconf/emacs/general.el")
(load "~/myconf/emacs/buffers-utils.el")
(load "~/myconf/emacs/calendar.el")
(load "~/myconf/emacs/cond-mac-linux-win.el")
(load "~/myconf/emacs/duplicate-line.el")
(load "~/myconf/emacs/xah-cut-copy.el")
(load "~/myconf/emacs/keybindings.el")

;; --- Elisp related
(require 'hl-defined)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)
(package-initialize)


;;; basic.el ends here
