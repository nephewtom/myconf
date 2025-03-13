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

  :config
  (setq lsp-clients-clangd-executable "clangd")
  ;; (setq lsp-clients-clangd-args
  ;;       '(
  ;;         "--compile-commands-dir=D:/playground/raylib/trayimg/" ;; Set compile_commands.json location
  ;;         "--query-driver=C:/mingw-w64/x86_64-8.1.0-win32-seh-rt_v6-rev0/mingw64/bin/g++.exe"
  ;;         "--header-insertion=never"
  ;;         ))

  (add-hook 'lsp-mode-hook (lambda () (eldoc-mode -1))))



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

