;; --- Company ---
(use-package company
  :init
  (global-company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  :ensure t
  :config
  (company-mode)
  (setq company-idle-delay 0)
  (setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
  (add-to-list 'company-backends 'company-c-headers) ;; Backend for header files
  (add-to-list 'company-backends 'company-elisp)
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    ;; remove
    (setq company-backends (delete 'company-clang company-backends))
    ;; add
    ;;(add-to-list 'company-backends 'company-dabbrev)
    )
  
  :bind (:map company-search-map  
              ("C-t" . company-search-toggle-filtering)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))
