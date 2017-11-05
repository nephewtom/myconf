(global-auto-revert-mode t) ;; automatically revert buffer when file changes

(defun switch-to-previous-buffer ()
  "Swap to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun indent-buffer ()
  "Select current buffer and indent it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


(toggle-uniquify-buffer-names) ;; Different buffer name for same name files

;; Ibuffer
;;(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1))) ;; Update ibuffer automatically

;;
;; https://emacs.stackexchange.com/questions/2177/how-can-i-make-ibuffer-auto-refresh-the-list-of-buffers/2179?noredirect=1#comment52199_2179
(defun my-ibuffer-stale-p (&optional noconfirm)
  ;; let's reuse the variable that's used for 'ibuffer-auto-mode
  (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))

(defun my-ibuffer-auto-revert-setup ()
  (set (make-local-variable 'buffer-stale-function)
       'my-ibuffer-stale-p)
  (set (make-local-variable 'auto-revert-verbose) nil)
  (auto-revert-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-auto-revert-setup)


;; --- Buffers & Ibuffer stuff ---
;; Remove from Ibuffers the buffers that match these regexp
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates " .*") ;; Check my comments and Sigma one in how-can-i-make-ibuffer-auto-refresh...
(add-to-list 'ibuffer-never-show-predicates "^\\*helm ag")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm mini")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm find")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm grep exts")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm M-x")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm-mode")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm buffers")
(add-to-list 'ibuffer-never-show-predicates "^\\*helm man woman*")
(add-to-list 'ibuffer-never-show-predicates "^\\*Helm Swoop")
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
(add-to-list 'ibuffer-never-show-predicates "^\\*Disabled")
(add-to-list 'ibuffer-never-show-predicates "^\\*Help")
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp")
(add-to-list 'ibuffer-never-show-predicates "^\\*JDEE")
(add-to-list 'ibuffer-never-show-predicates "^\\*magit-process")
(add-to-list 'ibuffer-never-show-predicates "^\\*magit-diff")
(add-to-list 'ibuffer-never-show-predicates "^\\*magit-log")
