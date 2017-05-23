(load "~/myconf/emacs/general.el")
(load "~/myconf/emacs/buffers-utils.el")
(load "~/myconf/emacs/calendar.el")
(load "~/myconf/emacs/cond-mac-linux-win.el")
(load "~/myconf/emacs/duplicate-line.el")
(load "~/myconf/emacs/xah-cut-copy.el")
(load "~/myconf/emacs/keybindings.el")
(load "~/myconf/emacs/term.el")
(load "~/myconf/emacs/compilation.el")
(load "~/myconf/emacs/elisp.el")

(package-initialize)

;; It allows you to move the current line using M-up / M-down
;; If a region is marked, it will move the region instead.
(require 'move-text)
(move-text-default-bindings)

(load "~/myconf/emacs/smart-line.el")

;; not working if only loading this basic.el file
;; probably need the require package & refresh
(load "~/myconf/emacs/company.el")

;;; basic.el ends here

;; This is brokin in parte 
