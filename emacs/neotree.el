;; (From https://www.emacswiki.org/emacs/NeoTree )
;; Useful keybindings
;; ------------------
;; n : next line
;; p : previous line 
;; SPC/TAB : Fold/Unfold current item if it is a directory.
;; SPC : Open current item if it is a file.
;; ENTER : change root directory to current one
;; . / U : go up a directory
;; A : Maximize/Minimize NeoTree window 
;; d : open in dired
;; q : hide NeoTree

(use-package neotree
  :ensure
  :config
  (setq neo-window-fixed-size nil)
  ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  :bind (:map neotree-mode-map
              ("RET" . 'neotree-change-root)
              ("." . 'neotree-select-up-node)
              )
  )

(defun neo-fit (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

;;(add-hook 'neo-change-root-hook #'neotree-resize-window)
;;(add-hook 'neo-enter-hook #'neotree-resize-window)
