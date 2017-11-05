;;; init.el starts here

(if (not (boundp 'basic-conf))
    (load "~/myconf/emacs/basic.el"))

;; TODO: Check /use-package/ module...
;; TODO: Autoload preferred
;; TODO: Check startup profilers:
;; https://oremacs.com/2015/02/24/emacs-speed-test/
;; https://github.com/jschaf/esup
;; https://github.com/dholm/benchmark-init-el

(require 'package)
;; --- Packages ELPA, MELPA, Marmalade ---
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))

(load "~/myconf/emacs/dired.el")
(load "~/myconf/emacs/xah-lookup.el")
(load "~/myconf/emacs/edit-with-emacs.el")
(load "~/myconf/emacs/nxml.el")
(load "~/myconf/emacs/hideshow.el")
(load "~/myconf/emacs/markdown.el")
(load "~/myconf/emacs/helm.el")
(load "~/myconf/emacs/magit.el")
(load "~/myconf/emacs/python.el")
(load "~/myconf/emacs/org-mode.el")

(setq custom-file "~/myconf/emacs/custom.el")
(load custom-file)
(put 'scroll-left 'disabled nil)

(server-start) ;; emacs server
;;; init.el ends here
