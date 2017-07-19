;; --- Dired ---
;; TODO: Sort dired by time date as default 
;; https://superuser.com/questions/875241/emacs-dired-sorting-by-time-date-as-default

(setq diredp-hide-details-initially-flag nil)
(require 'dired+)
(setq dired-listing-switches "-lkt")
;; move to up directory with '.'
(define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
;; Following key is already binded to 'a'
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "M-p") 'backward-paragraph)
;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq dired-auto-revert-buffer t)

(put 'dired-find-alternate-file 'disabled nil)
