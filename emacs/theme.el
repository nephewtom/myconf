(load-theme 'monokai t)

;;(load-theme 'naysayer t)
(set-face-attribute 'mode-line-inactive nil :box t)
;; (custom-theme-set-variables
;;  'naysayer
;;  '(linum-format "%5i ")
;;  )

;; org-mode faces
(custom-set-faces
 ' (org-level-1 ((t (:inherit outline-1 :foreground "#FD971F")))))

(custom-set-faces
 ' (org-level-2 ((t (:inherit outline-2 :foreground "#A6E22E")))))

(custom-set-faces
 ' (org-level-3 ((t (:inherit outline-3 :foreground "#66D9EF")))))

(custom-set-faces
 ' (org-level-4 ((t (:inherit outline-4 :foreground "#E6DB74")))))

(custom-set-faces
 ' (org-level-5 ((t (:inherit outline-5 :foreground "#A1EFE4")))))

(custom-set-faces
 ' (org-level-6 ((t (:inherit outline-6 :foreground "#A6E22E")))))

(custom-set-faces
 ' (org-level-7 ((t (:inherit outline-7 :foreground "#F92672")))))

(custom-set-faces
 ' (org-level-8 ((t (:inherit outline-8 :foreground "#66D9EF")))))



;; https://stackoverflow.com/q/9990370/316232
(global-hl-line-mode t) ;; highlight current line
(make-variable-buffer-local 'global-hl-line-mode)

;; current line highlighted color
(set-face-background hl-line-face "#404040")

;; region highlight color
(set-face-attribute 'region nil :background "#848000") ;;

;; fringe color (between line numbers and buffer)
(set-face-attribute 'fringe nil :background "#505050")

