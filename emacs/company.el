(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


(defun my-lsp-newline ()
  "Custom function to override ENTER behavior in LSP buffers."
  (interactive)
  (insert-char ?\n)
  (indent-according-to-mode))

;; --- LSP Mode ---
(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         ;; (python-mode . lsp)
         ;; (js-mode . lsp)
         ;; Add more languages as needed, but NOT `emacs-lisp-mode`
         (lsp-mode . lsp-enable-which-key-integration)) ;; Show key hints

  :init
  (setq lsp-diagnostics-provider :flycheck)

  :bind (:map lsp-mode-map
              ("RET" . my-lsp-newline)
              ("M-RET" . lsp-execute-code-action))
  
  :config
  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp/insert-text-mode-adjust-indentation 4)

  (unbind-key "M-n" lsp-signature-mode-map)
  (unbind-key "M-p" lsp-signature-mode-map)
  
  ;; TOM Testing
  (setq lsp-completion-provider :none)      ;; Disable LSP completion provider (CAPF or others)
  (setq lsp-completion-provider :capf)




  (add-hook 'lsp-mode-hook (lambda () (eldoc-mode -1)))

  (add-hook 'lsp-mode-hook
            (lambda ()
              (local-set-key (kbd "M-<f9>") 'flycheck-list-errors)
              (local-set-key (kbd "M-<f5>") 'flycheck-next-error)
              (local-set-key (kbd "M-<f6>") 'flycheck-previous-error)
              ))


  (put 'lsp-clients-clangd-args 'safe-local-variable #'listp)

  ;; (setq lsp-clients-clangd-args
  ;;       '(
  ;;         "--compile-commands-dir=D:/playground/raylib/trayimg/" ;; Set compile_commands.json location
  ;;         "--query-driver=C:/mingw-w64/x86_64-8.1.0-win32-seh-rt_v6-rev0/mingw64/bin/g++.exe"
  ;;         "--header-insertion=never"
  ;;         ))

  )


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
