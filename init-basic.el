;;; package --- Emacs init-basic.el file
;;; Commentary:

;;; Code:

;; --- General configuration ---

(server-start) ;; emacs server
(setq inhibit-startup-message t)
(tool-bar-mode -1) ;; removes tool-bar
(require 'iso-transl) ;; Make dead keys work
(cua-mode t) ;; Ctrl+Z, Ctrl+X, Ctrl+C, Ctrl+V (Cmd+ in Mac OSX)

(column-number-mode t)
(show-paren-mode 1)
(electric-pair-mode 1)
(defvar electric-pair-pairs) ;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '( (?\" . ?\") (?\{ . ?\}) ) )
(electric-indent-mode 1)

(global-linum-mode t) ;; line numbers in all buffers
(global-hl-line-mode t) ;; highlight current line
(set-face-background hl-line-face "#FBFFAB") ;; yellowish color
(set-face-attribute 'fringe nil :background "#d0f0ff") ;; Color blue en sangrado
;;(blink-cursor-mode 1) ;;

(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
;; Reminder! To untabify a whole buffer, mark the whole buffer and
;; use: M-x untabify
(global-set-key (kbd "<f6>") 'mark-whole-buffer)
;; TODO: When switch-to-previous-buffer (f8), this says 'Mark set',
;; and I need to hit F6 twice... Test it with emacs -q... and how to fix it

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode t) ;; automatically revert buffer when file changes

;; changes default Emacs behaviour, allowing to delete without kill-ring & inserting over selection.
(delete-selection-mode 1)
(setq default-directory "~")

;; frame title
(setq frame-title-format '("nephewtom" ": "(:eval (if (buffer-file-name)
                                                      (buffer-file-name) "%b"))))
;; Set unlimited buffer size for terminal
(setq term-buffer-maximum-size 0)

;; Check: http://emacs.stackexchange.com/questions/22621/cutting-selection-with-cua-mode-bindings-after-searching/
(define-key isearch-mode-map (kbd "C-x") nil)

;; --- Cut, Copy with universal arguments without selection  ---

(defun dd-like-vim (arg)
  "Emulates yy command on vim, copy lines (as many as ARG = prefix argument)."
  (interactive "p")
  (kill-region (line-beginning-position)
               (line-beginning-position (+ 1 arg)))
  (message "%d line%s cut" arg (if (= 1 arg) "" "s")))
(global-set-key (kbd "C-x C-x") 'dd-like-vim)

(defun yyank-like-vim (arg)
  "Emulates yy command on vim, copy lines (as many as ARG = prefix argument)."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; From: http://emacswiki.org/emacs/CopyingWholeLines
(global-set-key (kbd "C-y") 'yyank-like-vim)

(global-set-key (kbd "<f2>") 'dd-like-vim)
(global-set-key (kbd "<f3>") 'yyank-like-vim)
(global-set-key (kbd "<f4>") 'yank)

;; TODO: check http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html


;; --- Useful functions and bindings for buffers ---
(global-set-key [C-tab] 'other-window)

(defun switch-to-previous-buffer ()
  "Swap to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "<f8>") 'switch-to-previous-buffer)
(global-set-key (kbd "C-o") 'switch-to-previous-buffer)

(defun indent-buffer ()
  "Select current buffer and indent it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "<f11>") 'indent-buffer) ;; Personal taste

(defun move-end-of-line-newline-and-indent ()
  "Insert a newline, then indent according to major mode."
  (interactive "*")
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))
(global-set-key (kbd "C-j") 'move-end-of-line-newline-and-indent)

(toggle-uniquify-buffer-names) ;; Different buffer name for same name files


;; --- Personal rebinding of common keys ---

(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w C-w") 'kill-this-buffer) ;; Just like Chrome, etc..

;; Similar to vim as they may remind me so...
(global-set-key (kbd "M-j") 'join-line) ;; Almost like J in vim (joins to previos line)
;; join-line is a defalias of delete-indentation.
(global-set-key (kbd "M-z") 'recenter-top-bottom) ;; Almost as zz zt...
(global-set-key (kbd "C-.") 'repeat) ;; Like . in vim?

;; Scroll up & down in M-n & M-p
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-n") 'scroll-up-command)

;; Same position as US keyboard layout (M-\) [US keyboard has \, Spanish ç]
(global-set-key (kbd "M-ç") 'delete-horizontal-space)
;; http://stackoverflow.com/questions/445225/emacs-command-to-delete-up-to-non-whitespace-character
;; In that SO question says to use delete-indentation function
;; Tomi, what is the difference between both?

(global-set-key (kbd "C-S-r") 'query-replace) ;; Seems to remind me r=replace
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f9>") 'hc-toggle-highlight-tabs)

(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)

;; Duplicate line
(defun duplicate-line (ARG)
  "Duplicate current line, ARG, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count ARG))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line ARG))

(global-set-key (kbd "C-l") 'duplicate-line)

;; Adjusting Split Pane Size
(global-set-key (kbd "C-x =") 'enlarge-window)
(global-set-key (kbd "C-x -") 'shrink-window)
(global-set-key (kbd "C-x +") 'shrink-window-if-larger-than-buffer)
(global-set-key (kbd "C-x _") 'balance-windows)


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
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "C-/") 'comment-eclipse)
(global-set-key (kbd "C-S-c") 'comment-eclipse)


;; --- Calendar stuff ---

(setq calendar-week-start-day 1)

;; Display week number
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-warning-face))

(setq calendar-intermonth-header
      (propertize "Wk"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'font-lock-keyword-face))


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


;;; init-basic.el ends here
