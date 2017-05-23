;; From: https://www.reddit.com/r/emacs/comments/2lf4un/how_do_you_make_emacs_work_for_development/
(require 'aggressive-indent) ;; Aggresive indentation
(aggressive-indent-global-mode)      ;; Enable aggressive indent mode everywhere

(require 'company)
(add-to-list 'company-backends 'company-c-headers)
(setq company-idle-delay 0)

(require 'hideshow)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; optional key bindings, easier than hs defaults

;; (eval-after-load "hs-minor-mode"
(define-key hs-minor-mode-map (kbd "C-c @ C-h") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-s") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-M-h") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-M-s") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-c") nil)
(define-key hs-minor-mode-map (kbd "C-c <left>") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c h") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c <right>") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c s") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c <up>") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c M-h") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c <down>") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-c M-s") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-c C-c") 'hs-toggle-hiding)


;; C/C++ Config
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

