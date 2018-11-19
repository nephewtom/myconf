;; basic.el starts here

;; See: https://emacs.stackexchange.com/a/5888/6957
(setq package-enable-at-startup nil)
(package-initialize)

(load "~/myconf/emacs/general.el")
(load "~/myconf/emacs/column-and-line-numbers.el")
(load "~/myconf/emacs/paren-indent.el")
(load "~/myconf/emacs/calendar.el")
(load "~/myconf/emacs/cond-mac-linux-win.el")
(load "~/myconf/emacs/duplicate-line.el")
(load "~/myconf/emacs/xah-cut-copy.el")
(load "~/myconf/emacs/compilation.el")
(load "~/myconf/emacs/sudo.el")
(load "~/myconf/emacs/defalias.el")
(load "~/myconf/emacs/old/dired.el")
(load "~/myconf/emacs/dired.el")
(load "~/myconf/emacs/ediff.el")


(load "~/myconf/emacs/old/company.el")
(company-mode)

(load "~/myconf/emacs/old/term.el")
(load "~/myconf/emacs/old/buffers-utils.el")
(load "~/myconf/emacs/old/movement.el")
(load "~/myconf/emacs/old/elisp.el")

(load "~/myconf/emacs/keybindings.el")

(defvar basic-conf t
  "t if my personal basic Elisp configuration was loaded")

(message "Emacs ready with basic.el !")
;;; basic.el ends here
