;; --- Helm ---
;; * helm-find: C-x c /
;; * To find from helm-find-files (C-x C-f), press: C-c /
;; * To grep from helm-find-files (C-x C-f), press: C-u C-s
;; * helm-do-grep:
;;
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; TODO: Understand what is this for...
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "M-x")  'helm-select-action) ; list actions using M-x again

(setq helm-split-window-in-side-p nil)

(define-key helm-map (kbd "M-n")  'helm-next-page)
(define-key helm-map (kbd "M-p")  'helm-previous-page)
(define-key helm-map (kbd "C-v")  'helm-yank-text-at-point)
(define-key helm-map (kbd "C-<up>")  'previous-history-element)
(define-key helm-map (kbd "C-<down>")  'next-history-element)

(helm-autoresize-mode 1)
;;(setq helm-display-header-line nil) ;; t by default
;;(set-face-attribute 'helm-source-header t :height 10.0)

