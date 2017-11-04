(elpy-enable)

;; What about these two if I use virtualenv ?
(elpy-use-ipython "ipython3")
(setq elpy-rpc-python-command "python3")

(setq elpy-rpc-backend "jedi")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

