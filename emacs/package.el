(require 'package)
(setq package-enable-at-startup nil)

;; https://www.reddit.com/r/emacs/comments/1cc2205/failed_to_download_gnu_archive_in_emacs_28/
(setq package-check-signature nil)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
;; installed by default from Emacs 28 onwards


                                        ; activate all the packages (in particular autoloads)
(package-initialize)


;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ;; fetch the list of packages available 
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; list the packages you want
(setq package-list '(use-package diminish edit-server buffer-move monokai-theme))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(eval-when-compile
  (require 'use-package))
;; (setq use-package-always-ensure t) ;; Ensures all use-package declarations install by default

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package iedit
  :ensure t)

                                        ; set the path for manually installed packages
(add-to-list 'load-path "~/.emacs.d/packages")

