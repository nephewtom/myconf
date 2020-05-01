(use-package cc-mode
  :config
  (unbind-key "C-M-h" c++-mode-map)
  (dumb-jump-mode)
  )


;; https://www.reddit.com/r/emacs/comments/2lf4un/how_do_you_make_emacs_work_for_development/
(require 'aggressive-indent) ;; Aggresive indentation
(aggressive-indent-global-mode)      ;; Enable aggressive indent mode everywhere
;;(which-function-mode)

(setq c-default-style "stroustrup") ;; BSD/Allman brackets
(setq c-basic-offset 4)      ;; 4-space indent
;;(add-hook 'c-mode-common-hook 'flycheck-color-mode-line-mode)


;; (Conditional) C/C++ Keybinds
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "M-o") 'ff-find-other-file)))
