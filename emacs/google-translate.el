(use-package google-translate
  :ensure t
  :config
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "es")
  ;; (global-set-key (kbd "C-<f11>") 'google-translate-at-point)
  ;; (global-set-key (kbd "C-<f10>") 'google-translate-at-point-reverse)
  (global-set-key (kbd "C-<f8>") 'google-translate-at-point)
  (global-set-key (kbd "C-<f7>") 'google-translate-at-point-reverse)
  (require 'google-translate-default-ui)
  )
