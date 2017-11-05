;; --- helm spotify has errors if called without debug-on-error set.
(defun tom-spotify ()
  "Wrapper for calling spotify from keyboard shortcut and removing possibility for error."
  (interactive)
  (setq debug-on-error t)
  (helm-spotify)
  (setq debug-on-error nil))t

