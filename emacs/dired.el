;; --- Dired ---
(require 'dired )
(setq dired-listing-switches "-lk")
;; move to up directory with '.'
(define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
;; Following key is already binded to 'a'
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq dired-auto-revert-buffer t)

(put 'dired-find-alternate-file 'disabled nil)
