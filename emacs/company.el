;; --- Company ---
(require 'company)
(setq company-idle-delay 0)
(setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
(add-to-list 'company-backends 'company-c-headers) ;; Bakckend for header files

(add-hook 'after-init-hook 'global-company-mode)

(define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
