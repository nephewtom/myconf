(use-package super-save
  :ensure t
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 5)
  (super-save-mode 1)

  ;; Manually add an idle timer
  (run-with-idle-timer super-save-idle-duration t #'super-save-command))
