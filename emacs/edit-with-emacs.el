;; --- Edit with Emacs ---

;; Stuff to edit content in web forms via "Edit with Emacs" Chrome plugin
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)

  :config
  (add-to-list 'edit-server-url-major-mode-alist '("^stackoverflow" . markdown-mode))
  (add-to-list 'edit-server-url-major-mode-alist '("^emacs\\.stackexchange" . markdown-mode))
  (add-to-list 'edit-server-url-major-mode-alist '("^unix\\.stackexchange" . markdown-mode)))

;; When using firefox plugin itsalltext with Emacs, finish editing on Emacs with C-x #
;; http://psung.blogspot.com.es/2009/05/using-itsalltext-with-emacsemacsclient.html
