;; --- Hideshow ---
(use-package hideshow
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'nxml-mode-hook 'hs-minor-mode)
  (add-hook 'html-mode-hook 'hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c @ C-h" . nil)
              ("C-c @ C-s" . nil)
              ("C-c @ C-M-h" . nil)
              ("C-c @ C-M-s" . nil)
              ("C-c @ C-c" . nil)
              ("C-c <left>" . hs-hide-block)
              ("C-c h" . hs-hide-block)
              ("M-]" . hs-hide-block)
              ("C-c <right>" . hs-show-block)
              ("C-c s" . hs-show-block)
              ("M-[" . hs-show-block)
              ("C-c <up>" . hs-hide-all)
              ("C-c M-h" . hs-hide-all)
              ("C-c <down>" . hs-show-all)
              ("C-c M-s" . hs-show-all)
              ("C-c C-c" . hs-toggle-hiding))
  :config
  (require 'hideshowvis)
  (hideshowvis-minor-mode)
  (hideshowvis-symbols)
  )

;; optional key bindings, easier than hs defaults
;;(define-key html-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;; Yafolding shows a triangule at the left... Cool!

;; TODO: Check yafolding mode. It looks very simple and easy to use.
;; TODO: It has very easy key-bindings, though C-RET collide with cua-set-rectangle-mark
;; TODO: Check: https://github.com/zenozeng/yafolding.el
;; --- Bindings from yafolding.el ---
;; (defvar yafolding-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
;;     (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
;;     (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
;;     map))
