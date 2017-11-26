;; --- Columns, line-numbers, etc.
(column-number-mode t)
(global-linum-mode t) ;; line numbers in all buffers

;; https://stackoverflow.com/questions/9990370/how-to-disable-hl-line-feature-in-specified-mode
(global-hl-line-mode t) ;; highlight current line
(make-variable-buffer-local 'global-hl-line-mode)

(set-face-background hl-line-face "#FFCA00") ;; orangewish color
;;(set-face-background hl-line-face "#fbffab") ;; Test it here
(set-face-attribute 'fringe nil :background "#d0f0ff") ;; Color blue en sangrado
;;(set-face-attribute 'fringe nil :background "gray30") ;; Test it here
