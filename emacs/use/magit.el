;; --- Git ---
(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")

  (add-hook 'magit-mode-hook (lambda ()
                               (define-key magit-mode-map (kbd "C-<tab>") nil)
                               (define-key magit-mode-map (kbd "<backtab>") 'magit-section-cycle)
                               (define-key magit-mode-map (kbd "C-w") 'kill-this-buffer)
                               (define-key magit-mode-map (kbd "M-1") nil)
                               (define-key magit-mode-map (kbd "M-2") nil)
                               (define-key magit-mode-map (kbd "M-3") nil)
                               (define-key magit-mode-map (kbd "M-4") 'magit-section-show-level-2-all)
                               )))
