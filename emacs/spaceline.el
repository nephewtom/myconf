;; http://amitp.blogspot.com.es/2017/01/emacs-spaceline-mode-line.html
(use-package spaceline
  :ensure t)
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme)
  )
;;(spaceline-spacemacs-theme)



;; "❖ %s"
