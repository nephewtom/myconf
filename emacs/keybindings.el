(require 'iso-transl) ;; Make dead keys work

;; --- Cua mode 
(cua-mode t) ;; Ctrl+Z, Ctrl+X, Ctrl+C, Ctrl+V (Cmd+ in Mac OSX)
(define-key isearch-mode-map (kbd "C-x") nil)
;; Check: http://emacs.stackexchange.com/questions/22621/cutting-selection-with-cua-mode-bindings-after-searching/


;; --- Scroll up & down
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-n") 'scroll-up-command)


;; --- FX keys
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'mark-whole-buffer)
;; TODO: When switch-to-previous-buffer (f6), this says 'Mark set',
;; and I need to hit F6 twice... Test it with emacs -q... and how to fix it
(global-set-key (kbd "<f7>") 'ibuffer)
(global-set-key (kbd "<f8>") 'switch-to-previous-buffer)
(global-set-key (kbd "<f9>") 'hc-toggle-highlight-tabs)
(global-set-key (kbd "<f11>") 'indent-buffer) ;; Personal taste

;; --- Buffers & windows
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'kill-this-buffer) ;; Just like Chrome, etc..
(global-set-key (kbd "C-o") 'switch-to-previous-buffer)

(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)


;; --- Font-size & split-pane size
(global-set-key (kbd "C-=") 'text-scale-adjust)

;; (global-set-key (kbd "C-x =") 'enlarge-window)
;; (global-set-key (kbd "C-x -") 'shrink-window)
;; (global-set-key (kbd "C-x +") 'shrink-window-if-larger-than-buffer)
;; (global-set-key (kbd "C-x _") 'balance-windows)


;; --- Comments like Eclise ---
(defun comment-eclipse ()
  "Emulate comment code as Eclipse does it."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
;; --- Comments like Eclise ---
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "C-/") 'comment-eclipse)
(global-set-key (kbd "C-S-c") 'comment-eclipse)


;; --- Miscellaneous
(global-set-key (kbd "C-S-r") 'query-replace) ;; Seems to remind me r=replace

(global-set-key (kbd "C-j") 'join-line) ;; Almost like J in vim (joins to previos line)
;; TODO: Is C-j different for elisp mode?
;; join-line function is a defalias of delete-indentation.

(global-set-key (kbd "C-.") 'repeat) ;; Like . in vim?

(global-set-key (kbd "M-'") 'delete-horizontal-space)
;; Same position as US keyboard layout (M-\) [US keyboard has \, Spanish ç]
;; http://stackoverflow.com/questions/445225/emacs-command-to-delete-up-to-non-whitespace-character
;; In that SO question says to use delete-indentation function
;; Tomi, what is the difference between both?
