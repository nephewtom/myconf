;; --- General
(server-start) ;; emacs server
(setq inhibit-startup-message t)
(tool-bar-mode -1) ;; removes tool-bar
(setq default-directory "~/")
(setq frame-title-format '("nephewtom" ": "(:eval (if (buffer-file-name)
                                                      (buffer-file-name) "%b"))))

;; --- Columns, line-numbers, etc.
(column-number-mode t)
(global-linum-mode t) ;; line numbers in all buffers
(global-hl-line-mode t) ;; highlight current line
(set-face-background hl-line-face "#FBFFAB") ;; yellowish color
(set-face-attribute 'fringe nil :background "#d0f0ff") ;; Color blue en sangrado


;; --- Paren stuff
(show-paren-mode 1)
(electric-pair-mode 1)
(defvar electric-pair-pairs) ;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '( (?\" . ?\") (?\{ . ?\}) ) )
(electric-indent-mode 1) ;; indent after enter

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %.  ARG."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren) ;; Like vim


;; --- Miscellaneous
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(delete-selection-mode 1) ;; Allows to delete without kill-ring & inserting over selection.
(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame
(setq ediff-split-window-function 'split-window-horizontally)


;; --- Move end of line / Join line
(defun move-end-of-line-newline-and-indent ()
  "Insert a newline, then indent according to major mode."
  (interactive "*")
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))


;; --- Package initialize
(setq package-enable-at-startup nil)
(package-initialize)


;; --- Scroll bar ---
(scroll-bar-mode t)

