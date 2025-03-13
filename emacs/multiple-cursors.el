;; https://github.com/magnars/multiple-cursors.el

;; TO TEST THIS, USE THE FILE: multiple-cursors.playground

(use-package multiple-cursors
  :ensure t
  :bind (("C-:" . mc/edit-lines)           ;; Add cursor to each line in selection
         ("M-;" . mc/mark-next-like-this)  ;; Add cursor to next occurrence
         ("M-/" . mc/mark-previous-like-this) ;; Add cursor to previous occurrence
         ("M-?" . mc/mark-all-like-this))) ;; Add cursors to all occurrences


(defun my/disable-c-electric-brace ()
  "Disable `c-electric-brace` in multiple-cursors mode."
  (local-set-key "{" 'self-insert-command)
  (local-set-key "}" 'self-insert-command))

(defun my/enable-c-electric-brace ()
  "Restore `c-electric-brace` when multiple-cursors mode is disabled."
  (local-set-key "{" 'c-electric-brace)
  (local-set-key "}" 'c-electric-brace))

(add-hook 'multiple-cursors-mode-hook
          (lambda ()
            (if multiple-cursors-mode
                (my/disable-c-electric-brace)
              (my/enable-c-electric-brace))))
