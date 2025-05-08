;; Tell Emacs where to save compiled files
;; (when (boundp 'native-comp-eln-load-path)
;;   (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; activate all the packages (in particular autoloads)
(package-initialize)

(setq package-check-signature nil)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; list the packages you want
(setq package-list '(use-package diminish edit-server buffer-move monokai-theme))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

;; set the path for manually installed packages
(add-to-list 'load-path "~/.emacs.d/packages")

(setq native-comp-async-report-warnings-errors nil)
