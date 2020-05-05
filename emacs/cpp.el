(use-package cc-mode
  :config
  (unbind-key "C-M-h" c++-mode-map)
  (setq c-default-style "stroustrup") ;; BSD/Allman brackets
  (setq c-basic-offset 4)      ;; 4-space indent
  )


;; https://www.reddit.com/r/emacs/comments/2lf4un/how_do_you_make_emacs_work_for_development/
(require 'aggressive-indent) ;; Aggresive indentation
(aggressive-indent-global-mode)      ;; Enable aggressive indent mode everywhere
(which-function-mode)

;;(add-hook 'c-mode-common-hook 'flycheck-color-mode-line-mode)

;; (Conditional) C/C++ Keybinds
;; (add-hook 'c-mode-common-hook
;; (lambda () (local-set-key (kbd "M-o") 'ff-find-other-file)))

;; Stuff to run compiled programs from Emacs
(cond
 ((string-equal system-type "windows-nt")
  (message "System: Windows")
  (setq compile-command "build.bat")
  (defun run-program () (interactive)
         (async-shell-command "run.bat")
         (when (get-buffer "*run*")
           (kill-buffer "*run*"))
         (when (get-buffer "*compilation*")
           (kill-buffer "*compilation*"))
         (switch-to-buffer (get-buffer "*Async Shell Command*"))
         (rename-buffer "*run*")
         (delete-window (get-buffer-window (get-buffer "*run*")))
         (bury-buffer)
         )
  )
 
 (message "System: Other")
 )

(global-set-key (kbd "<f9>") 'run-program)

