(load-theme 'monokai t)

;; https://stackoverflow.com/q/9990370/316232
(global-hl-line-mode t) ;; highlight current line
(make-variable-buffer-local 'global-hl-line-mode)

;; current line highlighted color
(set-face-background hl-line-face "#404040")

;; region highlight color
(set-face-attribute 'region nil :background "#848000") ;;

;; fringe color (between line numbers and buffer)
(set-face-attribute 'fringe nil :background "#505050")
