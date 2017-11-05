;; basic.el starts here

;; See: https://emacs.stackexchange.com/a/5888/6957
(setq package-enable-at-startup nil)
(package-initialize)

(load "~/myconf/emacs/general.el")
(load "~/myconf/emacs/column-and-line-numbers.el")
(load "~/myconf/emacs/paren.el")
(load "~/myconf/emacs/movement.el")
(load "~/myconf/emacs/buffers-utils.el")
(load "~/myconf/emacs/calendar.el")
(load "~/myconf/emacs/cond-mac-linux-win.el")
(load "~/myconf/emacs/duplicate-line.el")
(load "~/myconf/emacs/xah-cut-copy.el")
(load "~/myconf/emacs/keybindings.el")
(load "~/myconf/emacs/defalias.el")
(load "~/myconf/emacs/term.el")
(load "~/myconf/emacs/compilation.el")
(load "~/myconf/emacs/elisp.el")
(load "~/myconf/emacs/ediff.el")

;;; basic.el ends here
