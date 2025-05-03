(use-package aggressive-indent
  :ensure t
  :hook (prog-mode . aggressive-indent-mode)) ;; Enable in all programming modes

(aggressive-indent-global-mode)


(defun my/flycheck-mode-line-status-text ()
  "Custom Flycheck status with colors: yellow for warnings, red for errors."
  (let ((error-count (flycheck-count-errors flycheck-current-errors)))
    (let ((errors (or (cdr (assq 'error error-count)) 0))
          (warnings (or (cdr (assq 'warning error-count)) 0)))
      (cond
       ((> errors 0)
        (propertize (format " FlyC:%d|%d" errors warnings)
                    'face 'error))
       ((> warnings 0)
        (propertize (format " FlyC:%d|%d" errors warnings)
                    'face 'warning))
       (t
        (propertize " FlyC:OK" 'face 'success))))))

(setq flycheck-mode-line
      '(:eval (my/flycheck-mode-line-status-text)))

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


;; --- Eglot ---
;; (use-package eglot
;;   :ensure t
;;   :hook ((c-mode . eglot-ensure)
;;          (c++-mode . eglot-ensure))
;;   :config
;;   (setq eglot-autoshutdown t)  ;; Automatically shutdown eglot when the buffer is killed
;;   ;; Optional: Set clangd as the LSP server for C/C++
;;   (setq eglot-server-programs '((c++-mode . ("clangd"))
;;                                 (c-mode . ("clangd"))))

;;   (setq eglot-stay-out-of '(flymake))

;;   (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1))) ;; Disable Eldoc
;;   (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;;   )



(use-package cc-mode
  :config
  (unbind-key "C-M-h" c++-mode-map)
  (unbind-key "C-M-j" c++-mode-map)
  (setq c-default-style "linux") ;; BSD/Allman brackets
  (setq c-basic-offset 4)      ;; 4-space indent

  (defun my/cpp-mode-keybindings ()
    (local-set-key (kbd "C-c o") 'ff-find-other-file)) ;; swith header/impl
  (add-hook 'c-mode-hook #'my/cpp-mode-keybindings)
  (add-hook 'c++-mode-hook #'my/cpp-mode-keybindings)
  
  :hook ((c-mode . (lambda () (eldoc-mode -1)))
         (c++-mode . (lambda () (eldoc-mode -1))))
  )


(which-function-mode)

;;(add-hook 'c-mode-common-hook 'flycheck-color-mode-line-mode)

;; (Conditional) C/C++ Keybinds
;; (add-hook 'c-mode-common-hook
;; (lambda () (local-set-key (kbd "M-o") 'ff-find-other-file)))


(use-package glsl-mode
  :ensure t
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode))
  :config
  ;; Enable line numbers for GLSL editing
  ;; (add-hook 'glsl-mode-hook #'display-line-numbers-mode)

  ;; Enable automatic indentation
  ;; (add-hook 'glsl-mode-hook #'electric-indent-mode)

  ;; Highlight TODO/FIXME comments
  (use-package hl-todo
    :ensure t
    :hook (glsl-mode . hl-todo-mode))

  ;; Enable automatic bracket pairing
  ;; (add-hook 'glsl-mode-hook #'electric-pair-mode)


  (defun run-glsl-viewer ()
    "Run glslViewer on the current GLSL shader file."
    (interactive)
    (when buffer-file-name
      (save-buffer)  ;; Save before running
      (start-process "glslViewer.exe" "*glslViewer*" "glslViewer" buffer-file-name)))

  ;; Ensure keybinding is only set after glsl-mode is loaded
  (with-eval-after-load 'glsl-mode
    (define-key glsl-mode-map (kbd "C-<f4>") 'run-glsl-viewer)))



(defun delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))


;; From: https://github.com/rexim/simpc-mode
(defun astyle-this-buffer ()
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number)))

(defalias 'ast 'astyle-this-buffer)
