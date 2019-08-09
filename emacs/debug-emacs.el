(setq package-enable-at-startup nil)
(package-initialize)
(setq sml/no-confirm-load-theme t)

(global-set-key [C-tab] 'other-window)
;; Uncomment to make Emacs crash
;;(sml/setup)
;;(gterm)


;; Disable all version control
(setq vc-handled-backends nil)

