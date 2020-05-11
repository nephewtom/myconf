(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "H-i") 'switch-to-buffer)

(eval-after-load "dired" '(progn
                            (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
                            (put 'dired-find-alternate-file 'disabled nil)
                            (define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
                            (define-key dired-mode-map (kbd "C-o") 'find-file)
                            (define-key dired-mode-map (kbd "C-w") 'kill-this-buffer)
                            (setq dired-listing-switches "-lkt")
                            )
                 )
(eval-after-load "ibuffer" '(progn
                              (define-key ibuffer-mode-map (kbd "C-o") nil)
                              (define-key ibuffer-mode-map (kbd "C-i") nil)
                              (define-key ibuffer-mode-map (kbd "M-h") 'toggle-ibuffer-groups)
                              (define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group)
                              )
                 )
