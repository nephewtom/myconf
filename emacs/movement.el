;; --- Move end of line / Join line
(defun move-end-of-line-newline-and-indent ()
  "Insert a newline, then indent according to major mode."
  (interactive "*")
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

;;; It allows you to move the current line using M-up / M-down
;; If a region is marked, it will move the region instead.
(use-package move-text
  :ensure t
  :config (move-text-default-bindings))


;; Avoid C-k and M-backspace overwrite clipboard
(defun my-backward-kill-word ()
  "Kill the previous word without overwriting the clipboard."
  (interactive)
  (let ((clipboard-content (gui-get-selection 'CLIPBOARD))) ;; Save clipboard
    (backward-kill-word 1)
    (gui-set-selection 'CLIPBOARD clipboard-content))) ;; Restore clipboard

(global-set-key (kbd "M-<backspace>") 'my-backward-kill-word)

(defun my-kill-line ()
  "Kill the line without overwriting the clipboard."
  (interactive)
  (let ((clipboard-content (gui-get-selection 'CLIPBOARD))) ;; Save clipboard
    (kill-line)
    (gui-set-selection 'CLIPBOARD clipboard-content))) ;; Restore clipboard

(global-set-key (kbd "C-k") 'my-kill-line)
