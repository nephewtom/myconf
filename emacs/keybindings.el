(require 'iso-transl) ;; Make dead keys work

;; NOTE: Do not bind C-y & M-w to anything.
;; NOTE: That way I keep their original function, in case I need it...
;; NOTE: M-w (kill-ring-save)
;; NOTE: C-y (yank) or (cua-paste)

;; --- Cua mode 
(cua-mode t) ;; Ctrl+Z, Ctrl+X, Ctrl+C, Ctrl+V (Cmd+ in Mac OSX)


;; --- Search with C-f like MOST apps...
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-forward-symbol-at-point)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-t") 'isearch-yank-word-or-char)
(define-key isearch-mode-map [down] 'isearch-repeat-forward)
(define-key isearch-mode-map [up] 'isearch-repeat-backward)
(global-set-key (kbd "C-s") 'save-buffer) ;; Use C-s to save

;; Check: http://emacs.stackexchange.com/questions/22621/cutting-selection-with-cua-mode-bindings-after-searching/
(define-key isearch-mode-map (kbd "C-x") nil)


;; --- Scroll up & down
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)


;; --- Macros
;; Differenciate <RET> from C-m 
;; https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
;; (define-key input-decode-map [?\C-m] [C-m])


(global-set-key (kbd "C-<f7>") 'kmacro-start-macro)
(global-set-key (kbd "C-<f8>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-<f9>") 'kmacro-call-macro)


;; --- Line operations
(global-set-key (kbd "M-j") 'move-end-of-line-newline-and-indent) ;; general.el
(global-set-key (kbd "M-h") 'join-line) 
;; I used to set it to C-j... in order to be similar to vi J key
;; join-line function is a defalias of delete-indentation.

(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-l") 'duplicate-line) ;; duplicate-line.el
(global-set-key (kbd "M-s M-s") 'delete-horizontal-space)
(global-set-key (kbd "M-s s") 'delete-horizontal-space)
;; Same position as US keyboard layout (M-\) [US keyboard has \, Spanish ç]
;; http://stackoverflow.com/questions/445225/emacs-command-to-delete-up-to-non-whitespace-character
;; In that SO question says to use delete-indentation function
;; Tomi, what is the difference between both?


;; --- Paren operations
;; (global-set-key "%" 'match-paren) ;; Like vim
;; (global-set-key "%" 'self-insert-command)
(global-set-key (kbd "C-M-j") 'down-list) ;; As C-M-u does backward-up-list


;; --- FX keys
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'mark-whole-buffer)
(global-set-key (kbd "<f7>") 'neotree-toggle)
(global-set-key (kbd "<f8>") 'ibuffer)
(global-set-key (kbd "<f9>") 'whitespace-mode)

;; F12 and M-F12 are defined in compilation.el
(global-set-key (kbd "<f11>") 'indent-buffer)

;; Open browser keys
(global-set-key (kbd "<f1> 7") 'browse-url-at-point)
(global-set-key (kbd "<f1> 6") 'browse-url-of-buffer)

(global-set-key (kbd "C-<f5>") 'start-file-manager)


;; --- Buffers
(defun switch-to-previous-buffer ()
  "Swap to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun indent-buffer ()
  "Select current buffer and indent it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-auto-revert-mode t) ;; automatically revert buffer when file changes


(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'kill-current-buffer) ;; Just like Chrome, etc..
(global-set-key (kbd "C-0") 'switch-to-previous-buffer)


;; --- Windows
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-3") 'split-window-below)
(global-set-key (kbd "M-o") 'find-file-other-window)

(defun split-window-right-and-other-window () "Does that" (interactive)
       (split-window-right)
       (other-window 1))
(global-set-key (kbd "M-2") 'split-window-right-and-other-window)
(global-set-key (kbd "M-0") 'delete-window)

(global-set-key [C-next] 'windmove-right)
(global-set-key [C-prior] 'windmove-left)



;; --- Font-size & split-pane size
(global-set-key (kbd "C-=") 'text-scale-adjust)

;; (global-set-key (kbd "C-x =") 'enlarge-window)
;; (global-set-key (kbd "C-x -") 'shrink-window)
;; (global-set-key (kbd "C-x +") 'shrink-window-if-larger-than-buffer)
;; (global-set-key (kbd "C-x _") 'balance-windows)


;; --- Comments
(defun comment-line-or-region ()
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
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "M-/") 'comment-dwim)
(global-set-key (kbd "C-/") 'comment-line-or-region)
(global-set-key (kbd "C-S-c") 'comment-line-or-region) ; Eclipse has this one too


;; --- Miscellaneous

(defun my/query-replace-from-region ()
  "Start `query-replace` with the selected region as the initial string."
  (interactive)
  (if (use-region-p)
      (let ((from (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (call-interactively `(lambda (to)
                               (interactive (list (read-string (format "Replace `%s` with: " from))))
                               (query-replace from to))))
    (call-interactively 'query-replace)))


(global-set-key (kbd "C-S-r") 'query-replace) ;; Seems to remind me r=replace
(global-set-key (kbd "C-.") 'repeat) ;; Like . in vim
(global-set-key (kbd "M-r") 'iedit-mode)

(global-set-key (kbd "M-y") 'company-complete)
;; (global-set-key (kbd "M-;") 'hippie-expand)
(global-set-key (kbd "C-;") 'company-files)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "M-z") 'recenter-top-bottom)

(global-set-key (kbd "M-<f4>") 'kill-emacs)
(global-set-key (kbd "<escape>") 'keyboard-quit)

(global-set-key (kbd "C-x C-S-e") 'eval-print-last-sexp)

;; --- Differenciate C-i & TAB
;; https://stackoverflow.com/q/1792326/316232

;; (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
;; ;; (global-set-key (kbd "C-i") (lambda () (interactive) (message "C-i")))
;; (global-set-key (kbd "<tab>") 'indent-for-tab-command)

;; Translate the problematic keys to the function key Hyper:
(keyboard-translate ?\C-i ?\H-i)
;; (global-set-key (kbd "<tab>") 'indent-for-tab-command)
(global-set-key (kbd "M-i") 'switch-to-buffer-other-window)
;;(define-key help-mode-map (kbd "<tab>") 'forward-button)

;; Paste with middle mouse button
;; https://stackoverflow.com/a/13043670/316232
(setq mouse-drag-copy-region t)
(setq select-active-regions nil)
(global-set-key [mouse-2] 'mouse-yank-at-click)

(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "H-i") 'switch-to-buffer)
