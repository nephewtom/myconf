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
