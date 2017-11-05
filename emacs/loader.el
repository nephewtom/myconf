;; loader.el starts here

;; TODO: Check /use-package/ module...
;; TODO: Autoload preferred
;; TODO: Check startup profilers:
;; https://oremacs.com/2015/02/24/emacs-speed-test/
;; https://github.com/jschaf/esup
;; https://github.com/dholm/benchmark-init-el

(server-start) ;; emacs server
(load "~/myconf/emacs/smart-line.el")
(load "~/myconf/emacs/company.el")
(company-mode)


(require 'package)
;; --- Packages ELPA, MELPA, Marmalade ---
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))

;; loader.el ends here
