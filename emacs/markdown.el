
;;(require 'markdown-mode) ;; Commented because it is breaking color-theme-print
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-open-command "haroopad")
(eval-after-load 'markdown-mode
  '(progn
     (define-key markdown-mode-map (kbd "C-c C-c t") 'markdown-toc/generate-toc)
     (define-key markdown-mode-map (kbd "M-p") nil)
     (define-key markdown-mode-map (kbd "M-n") nil)
     ))

;; From: https://github.com/shime/emacs-livedown
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

;;(global-set-key (kbd "C-M-m") 'livedown:preview)

;; From: http://increasinglyfunctional.com/2014/12/18/github-flavored-markdown-previews-emacs/
(setq markdown-command "/home/etomort/myconf/bin/flavor.rb")



