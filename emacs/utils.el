(defun clear-kill-ring ()
  "Clear the kill ring variable"
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect))
  (show-kill-ring))
(defalias 'ckr 'clear-kill-ring)

(defun show-kill-ring()
  (interactive)
  (describe-variable 'kill-ring))

(defalias 'skr 'show-kill-ring)

;; ABCEDFG
;; 12345
;; XYZ
