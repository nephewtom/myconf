(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map  python-mode-map
               ("C->" . python-indent-shift-right)
               ("C-<" . python-indent-shift-left))
  :config
  ;; (elpy-enable)
  ;; What about these two if I use virtualenv ?

  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  ;; (setq python-shell-interpreter "ipython3"
  ;;       python-shell-interpreter-args "-i --simple-prompt")

  ;; (setq elpy-rpc-python-command "python3")  
  ;; (setq elpy-rpc-backend "jedi") ;; Do I have to setq this?

  ;; (when (require 'flycheck nil t)
  ;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;;   (setq flycheck-python-flake8-executable "python3")
  ;;   (setq flycheck-python-pycompile-executable "python3")
  ;;   (setq flycheck-python-pylint-executable "python3")
  ;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

  ;; Fix, the run-python broke company-dabbrev-code completion
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; Force company-dabbrev-code back in Python buffers
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (derived-mode-p 'python-mode)
                    (setq-local company-backends '(company-dabbrev-code company-dabbrev)))))))
  )

;; I think jedi uses autocomplete-mode (AC) and not company-mode
;;(add-hook 'python-mode-hook 'jedi:setu)
;;(setq jedi:complete-on-dot t)

;; (setq elpy-rpc-python-command "python2")  
