;; C/C++ Config
;; From: https://www.reddit.com/r/emacs/comments/2lf4un/how_do_you_make_emacs_work_for_development/
(require 'aggressive-indent) ;; Aggresive indentation
(aggressive-indent-global-mode)      ;; Enable aggressive indent mode everywhere

(semantic-mode 1)            ;; CEDET holdover
(global-ede-mode 1)          ;; CEDET holdover
(setq c-default-style "bsd") ;; BSD/Allman brackets
(setq c-basic-offset 4)      ;; 4-space indent
(add-hook 'c-mode-common-hook 'company-mode)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook 'flycheck-color-mode-line-mode)
(add-hook 'c-mode-common-hook 'linum-mode)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'hideshowvis-minor-mode)

;; (Conditional) C/C++ Keybinds
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "<M-y>") 'company-complete)))
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "C-c j") 'find-tag)))
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "C-x C-o") 'ff-find-other-file)))


;; For ELisp
(require 'hl-defined)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)

