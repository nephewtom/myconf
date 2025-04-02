;; --- Helm ---
;; * helm-find: C-x c /
;; * To find from helm-find-files (C-x C-f), press: C-c /
;; * To grep from helm-find-files (C-x C-f), press: C-u C-s
;; * helm-do-grep:
;;
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-o" . helm-find-files)
         ("C-x b" . helm-mini)
         ("H-i" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ;; ("C-o" . helm-imenu)
         ("C-M-h" . helm-cscope-find-calling-this-function)
         ("C-M-." . helm-imenu)
         
         :map helm-map
         ;; TODO: Understand what these keys are for...
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ;; ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("M-x" . helm-select-action) ; list actions using M-x again

         ("M-n" . helm-next-page)
         ("M-p" . helm-previous-page)
         ("C-v" . helm-yank-text-at-point)
         ("C-<up>" . previous-history-element)
         ("C-<down>" . next-history-element)
         ("<escape>" . keyboard-escape-quit)
         
         :map helm-find-files-map
         ("C-r" . helm-ff-file-name-history)
         ("C-j" . helm-ff-run-find-sh-command)
         ;; ("C-g" . helm-ff-run-grep)         
         ("<escape>" . keyboard-escape-quit)
         )
  
  :config
  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (helm-mode 1)
  (setq helm-split-window-in-side-p t)
  (setq helm-split-window-inside-p t)
  (helm-autoresize-mode 1)

  (setq helm-move-to-line-cycle-in-source nil)

  (add-hook 'helm-after-action-hook
            (lambda ()
              (dolist (buffer (buffer-list))
                (when (string-match "\\`\\*helm" (buffer-name buffer))
                  (kill-buffer buffer)))))

  )




;; (use-package helm-config
;;   ;; :ensure t
;;   )

;;(setq helm-display-header-line nil) ;; t by default
;;(set-face-attribute 'helm-source-header t :height 10.0)

;; https://github.com/emacs-helm/helm/issues/1976
;;
;;(setq x-wait-for-event-timeout 0.1)

;; This makes Emacs on Windows unusable...
;;(setq x-wait-for-event-timeout nil)


;;(global-set-key (kbd "C-t") 'helm-grep-do-git-grep)
