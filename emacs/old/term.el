;; --- terminal stuff ---
(setq term-buffer-maximum-size 0) ;; Set unlimited buffer size for terminal

(defadvice term-line-mode (after term-line-mode-fixes ())
  "Enable cua and transient mark modes in term-line-mode."
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'transient-mark-mode) t))
(ad-activate 'term-line-mode)

(defadvice term-char-mode (after term-char-mode-fixes ())
  "Disable cua and transient mark modes in term-char-mode."
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)
