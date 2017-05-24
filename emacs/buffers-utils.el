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
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1))) ;; Update ibuffer automatically

;; --- Buffers & Ibuffer stuff ---
;; Remove from Ibuffers the buffers that match these regexp
(require 'ibuf-ext)
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

