;; Highlights do not work...
(setq pletter-highlights
      '(("Word:\\|Letters:\\|Guess:" . font-lock-function-name-face)
        ("WIN\\|LOSE" . font-lock-constant-face)))

(defun movechar-to-left ()
  (interactive)
  (if (< (current-column) 1)
      (error "Can't move to left more!")
    (kill-region (point) (+ (point) 1))
    (backward-char)
    (yank)
    (backward-char)))

(defun movechar-to-right ()
  (interactive)
  (if (= (current-column) (pos-of-last-char))
      (error "Can't move to right more!")
    (kill-region (point) (+ (point) 1))
    (forward-char)
    (yank)
    (backward-char)))

(defun pos-of-last-char ()
  "Position of last char in line."
  (interactive)
  (save-excursion
    (end-of-line)
    (- (current-column) 1)))

(defun pletter-end-of-line ()
  (interactive)
  (end-of-line)
  (goto-char (- (point) 1))
  )

;; Major mode
(defvar pletter-mode-keymap (make-sparse-keymap))
(define-key pletter-mode-keymap (kbd "C-q") 'move-pletter-mode)

(define-derived-mode pletter-mode fundamental-mode "pletter"
  "pletter-mode is a major mode for playing with letters and words.
\\{pletter-mode-map}"
  (use-local-map pletter-mode-keymap)
  (setq font-lock-defaults '(pletter-highlights))
  )

;; Minor mode
(defvar move-pletter-mode-keymap (make-sparse-keymap))
(define-key move-pletter-mode-keymap (kbd "<left>") 'movechar-to-left)
(define-key move-pletter-mode-keymap (kbd "<right>") 'movechar-to-right)
(define-key move-pletter-mode-keymap (kbd "C-e") 'pletter-end-of-line)

(define-minor-mode move-pletter-mode
  "Minor mode to toggle arrows bindings for pletter-mode."
  nil " move" move-pletter-mode-keymap
  (message "Toggle move-pletter")
  (message "%d" (pos-of-last-char))
  (if (> (current-column) (pos-of-last-char))
      (goto-char (- (point) 1))))

  (add-to-list 'auto-mode-alist '("\\.plt\\'" . pletter-mode))
(provide 'pletter-mode)
