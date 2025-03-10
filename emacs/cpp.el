(use-package cc-mode
  :config
  (unbind-key "C-M-h" c++-mode-map)
  (unbind-key "C-M-j" c++-mode-map)
  (setq c-default-style "linux") ;; BSD/Allman brackets
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

(defun delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))


;; From: https://github.com/rexim/simpc-mode
(defun astyle-this-buffer ()
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number)))

(defalias 'ast 'astyle-this-buffer)
