;; --- Company ---
(require 'company)
(setq company-idle-delay 0)
(setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
(add-to-list 'company-backends 'company-c-headers) ;; Bakckend for header files

(global-set-key (kbd "M-y") 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)
