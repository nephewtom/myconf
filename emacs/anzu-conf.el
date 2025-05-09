(use-package anzu
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key (kbd "M-%") 'anzu-query-replace-at-cursor)
  )
