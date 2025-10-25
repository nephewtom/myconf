(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; --- Company Mode ---
(use-package company
  :init
  (global-company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)

  :ensure t

  :config
  (company-mode)
  (setq company-idle-delay 0)
  (setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
  (add-to-list 'company-backends 'company-dabbrev) ;; Backend for header files
  

  :bind (:map company-search-map  
              ("C-t" . company-search-toggle-filtering)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(global-set-key (kbd "M-y") 'helm-company)


(defun helm-company-complete ()
  "Use helm to select company completions."
  (interactive)
  (when (company-manual-begin)
    (let ((helm-candidates (company-candidates)))
      (if helm-candidates
          (helm :sources (helm-build-sync-source "Company Completions"
                           :candidates helm-candidates
                           :action (lambda (candidate)
                                     (company-finish candidate)))
                :buffer "*helm-company*")
        (message "No completion candidates")))))
