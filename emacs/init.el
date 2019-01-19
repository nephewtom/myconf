;; Provide timestamp to *Messages* logs
(load "~/myconf/emacs/log.el")

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
(load "~/myconf/emacs/defalias.el")

(load "~/myconf/emacs/dired.el")
(load "~/myconf/emacs/ediff.el")
(load "~/myconf/emacs/helm.el")
(load "~/myconf/emacs/movement.el")
(load "~/myconf/emacs/buffers-utils.el")
(load "~/myconf/emacs/elisp.el")
(load "~/myconf/emacs/flycheck.el")
(load "~/myconf/emacs/company.el")
(load "~/myconf/emacs/xah-lookup.el")
(load "~/myconf/emacs/magit.el")

(load "~/myconf/emacs/term.el")
(load "~/myconf/emacs/org-mode.el")
(load "~/myconf/emacs/nxml.el")
(load "~/myconf/emacs/hideshow.el")
(load "~/myconf/emacs/edit-with-emacs.el")
(load "~/myconf/emacs/markdown.el")
(load "~/myconf/emacs/python.el")
;;(load "~/myconf/emacs/cpp.el")

(load "~/myconf/emacs/keybindings.el") ;; has (require 'iso-transl)
(load "~/myconf/emacs/cua.el")


(setq custom-file "~/myconf/emacs/custom.el")
(load custom-file)
(put 'scroll-left 'disabled nil)

;;(load "~/myconf/emacs/smart-line.el")
;;(load "~/myconf/emacs/spaceline.el")

(server-start) ;; emacs server
(message "Emacs ready with init.el !")
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
