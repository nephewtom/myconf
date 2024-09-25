;; Use keybindings
(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

;; Or using hooks
(use-package grip-mode
  :ensure t
  :hook ((markdown-mode org-mode) . grip-mode))
