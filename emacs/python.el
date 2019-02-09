(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (elpy-enable)
  ;; What about these two if I use virtualenv ?
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

  (setq elpy-rpc-python-command "python3")  
  (setq elpy-rpc-backend "jedi") ;; Do I have to setq this?

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; I think jedi uses autocomplete-mode (AC) and not company-mode
;;(add-hook 'python-mode-hook 'jedi:setu)
;;(setq jedi:complete-on-dot t)
