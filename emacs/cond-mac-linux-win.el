;; --- Font size & Mac OS X stuff ---

(cond
 ((string-equal system-type "darwin")
  ;; Mac stuff
  (setq mac-option-modifier 'command)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "M-w") 'kill-this-buffer) ;; this works on Mac too
  (define-key global-map (kbd "C-<f2>")
    (lambda ()
      (interactive)
      (x-popup-menu (list '(0 0) (selected-frame))
                    (mouse-menu-bar-map))))

  (set-face-attribute 'default nil :height 200))
 ;; Ubuntu stuff
 (
  (set-face-attribute 'default nil :height 140)
  ))
