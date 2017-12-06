;; --- Smart line ---
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
;  (sml/setup)
  (add-to-list 'rm-excluded-modes " MRev")
  (add-to-list 'rm-excluded-modes " ARev"))
