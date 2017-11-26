(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.text$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))

  :bind (("C-M-m" . livedown:preview)
         :map markdown-mode-map
         ("C-c C-c t" . markdown-toc/generate-toc)
         ("M-p" . nil)
         ("M-n" . nil))

  :config
  (setq markdown-open-command "haroopad")
  (setq markdown-command "/home/etomort/myconf/bin/flavor.rb"))

;; From: https://github.com/shime/emacs-livedown
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))

(use-package livedown
  ;;  :ensure t
  )
;; From: http://increasinglyfunctional.com/2014/12/18/github-flavored-markdown-previews-emacs/
