;;; package --- Emacs init.el file
;;; Commentary:

;;; Code:

(load "~/myconf/init-basic.el")
(load "~/myconf/init-packages.el")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain) t)
 '(flycheck-clang-language-standard nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(livedown:autostart nil)
 '(livedown:open t)
 '(livedown:port 1337)
 '(org-agenda-files (quote ("~/smip/SMIP-GOING.org")))
 '(org-startup-truncated nil)
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I/home/etomort/tomas/Corange/include" "-I/home/etomort/tomas/Corange/include/assets")
     (company-clang-arguments "-I/home/etomort/tomas/Corange/include/" "-I/home/etomort/tomas/Corange/include/assets/")
     (org-todo-keywords-faces
      ("IN-PROGRESS" . "red")
      ("TODO" . "blue")
      ("WAITING" . "yellow")
      ("DONE" . "green")
      ("CANCELED" . "black")))))
 '(scheme-program-name "petite" t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hc-tab ((t (:background "wheat"))))
 '(magit-item-highlight ((t nil)))
 '(sml/modes ((t (:inherit sml/global :foreground "dark violet" :weight bold)))))
