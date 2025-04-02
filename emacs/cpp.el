(use-package aggressive-indent
  :ensure t
  :hook (prog-mode . aggressive-indent-mode)) ;; Enable in all programming modes

(aggressive-indent-global-mode)


;; --- Eglot ---
(use-package eglot
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t)  ;; Automatically shutdown eglot when the buffer is killed
  ;; Optional: Set clangd as the LSP server for C/C++
  (setq eglot-server-programs '((c++-mode . ("clangd"))
                                (c-mode . ("clangd"))))

  (setq eglot-stay-out-of '(flymake))

  (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1))) ;; Disable Eldoc
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

  )



(use-package cc-mode
  :config
  (unbind-key "C-M-h" c++-mode-map)
  (unbind-key "C-M-j" c++-mode-map)
  (setq c-default-style "linux") ;; BSD/Allman brackets
  (setq c-basic-offset 4)      ;; 4-space indent
  ;; :bind (:map c-mode-map
  ;;             ("{" . my/c-electric-brace)
  ;;             ("}" . my/c-electric-brace))
  ;; :bind (:map c++-mode-map
  ;;             ("{" . my/c-electric-brace)
  ;;             ("}" . my/c-electric-brace))
  ;;
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
