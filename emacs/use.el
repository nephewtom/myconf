;; Try to speed up start-up
(setq-default gc-cons-threshold (* 100 1024 1024))
;;(let ((file-name-handler-alist nil)) "~/myconf/emacs/init.el")

;; Follow git symlinks
(setq vc-follow-symlinks t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(load "~/myconf/emacs/general.el")
(load "~/myconf/emacs/column-and-line-numbers.el")
(load "~/myconf/emacs/paren-indent.el")
(load "~/myconf/emacs/calendar.el")
(load "~/myconf/emacs/cond-mac-linux-win.el")
(load "~/myconf/emacs/duplicate-line.el")
(load "~/myconf/emacs/xah-cut-copy.el")
(load "~/myconf/emacs/compilation.el")
(load "~/myconf/emacs/sudo.el")
(load "~/myconf/emacs/ediff.el")
(load "~/myconf/emacs/defalias.el")
(load "~/myconf/emacs/keybindings.el") ;; has (require 'iso-transl)

(load "~/myconf/emacs/dired.el")

(load "~/myconf/emacs/use/term.el")
(load "~/myconf/emacs/use/buffers-utils.el")
(load "~/myconf/emacs/use/movement.el")
(load "~/myconf/emacs/use/elisp.el")
(load "~/myconf/emacs/use/flycheck.el")
(load "~/myconf/emacs/use/company.el")
(load "~/myconf/emacs/use/xah-lookup.el")
(load "~/myconf/emacs/use/edit-with-emacs.el")
(load "~/myconf/emacs/use/nxml.el")
(load "~/myconf/emacs/use/hideshow.el")
(load "~/myconf/emacs/use/markdown.el")
(load "~/myconf/emacs/use/helm.el")
(load "~/myconf/emacs/use/magit.el")
(load "~/myconf/emacs/use/python.el")
(load "~/myconf/emacs/use/org-mode.el")

;;(load "~/myconf/emacs/use/smart-line.el")

(setq custom-file "~/myconf/emacs/custom.el")
(load custom-file)
(put 'scroll-left 'disabled nil)

(load "~/myconf/emacs/use/spaceline.el")

(server-start) ;; emacs server
(message "Emacs ready with use.el !")
