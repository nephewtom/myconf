(global-auto-revert-mode t) ;; automatically revert buffer when file changes

(defun switch-to-previous-buffer ()
  "Swap to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun indent-buffer ()
  "Select current buffer and indent it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


(toggle-uniquify-buffer-names) ;; Different buffer name for same name files
