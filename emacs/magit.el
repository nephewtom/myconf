;; --- Git ---
(use-package magit
  :defer t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")

  (add-hook 'magit-mode-hook (lambda ()
                               (define-key magit-mode-map (kbd "C-<tab>") nil)
                               (define-key magit-mode-map (kbd "<backtab>") 'magit-section-cycle)
                               (define-key magit-mode-map (kbd "C-w") 'kill-this-buffer)
                               (define-key magit-mode-map (kbd "M-1") nil)
                               (define-key magit-mode-map (kbd "M-2") nil)
                               (define-key magit-mode-map (kbd "M-3") nil)
                               (define-key magit-mode-map (kbd "M-4") 'magit-section-show-level-2-all)
                               )))


;; WORKAROUND https://github.com/magit/magit/issues/2395
(define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
  "Mode for showing staged and unstaged changes."
  :group 'magit-status)

(defun magit-staging-refresh-buffer ()
  (magit-insert-section (status)
                        (magit-insert-untracked-files)
                        (magit-insert-unstaged-changes)
                        (magit-insert-staged-changes)))

(defun magit-staging ()
  (interactive)
  (magit-mode-setup #'magit-staging-mode))
