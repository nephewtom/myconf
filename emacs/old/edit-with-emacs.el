;; --- Edit with Emacs ---

;; Stuff to edit content in web forms via "Edit with Emacs" Chrome plugin
(require 'edit-server)
(edit-server-start)
(add-to-list 'edit-server-url-major-mode-alist '("^stackoverflow" . markdown-mode))
(add-to-list 'edit-server-url-major-mode-alist '("^emacs\\.stackexchange" . markdown-mode))
(add-to-list 'edit-server-url-major-mode-alist '("^unix\\.stackexchange" . markdown-mode))

(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;; When using firefox plugin itsalltext with Emacs, finish editing on Emacs with C-x #
;; http://psung.blogspot.com.es/2009/05/using-itsalltext-with-emacsemacsclient.html
