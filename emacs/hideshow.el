;; --- Hideshow ---
(require 'hideshow)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'nxml-mode-hook 'hs-minor-mode)
(add-hook 'html-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(define-key html-mode-map (kbd "C-c h") 'hs-toggle-hiding)

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

;; TODO: Check yafolding mode. It looks very simple and easy to use.
;; TODO: It has very easy key-bindings, though C-RET collide with cua-set-rectangle-mark
;; TODO: Check: https://github.com/zenozeng/yafolding.el
;; *** Bindings from yafolding.el ***
;; (defvar yafolding-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
;;     (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
;;     (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
;;     map))
