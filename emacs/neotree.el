(use-package neotree
  :ensure
  :config
  (setq neo-window-fixed-size nil)
  ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )

(defun neo-fit (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

(add-hook 'neo-change-root-hook #'neotree-resize-window)
(add-hook 'neo-enter-hook #'neotree-resize-window)
