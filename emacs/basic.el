
;; *** FILE:  begin.el
;; Provide timestamp to *Messages* logs
(load "~/myconf/emacs/log.el")

;; Backup files: https://www.johndcook.com/blog/emacs_windows/#backup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Try to speed up start-up
(setq-default gc-cons-threshold (* 100 1024 1024))
;;(let ((file-name-handler-alist nil)) "~/myconf/emacs/init.el")

;; Follow git symlinks
(setq vc-follow-symlinks t)


;; *** FILE:  bars-and-title.el
;; --- Bars & title
(setq inhibit-startup-message t)
(tool-bar-mode -1) ;; removes tool-bar
(menu-bar-mode -1) ;; removes tool-bar
(scroll-bar-mode t)
(setq default-directory "~/")
(setq frame-title-format '("tom@" (:eval (format "%s" system-type))
                           ": "(:eval (if (buffer-file-name)
                                          (buffer-file-name) "%b"))))

(fringe-mode '(16 . 0)) ;; Make left fringe 16 pixels and no right fringe

;; (setq initial-frame-alist
;;       '((background-color . "honeydew")))

;; (setq default-frame-alist
;;       '((background-color . "honeydew")))

;; --- Miscellaneous
(setq set-mark-command-repeat-pop t) ;; https://emacs.stackexchange.com/a/2818/6957
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(delete-selection-mode 1) ;; Allows to delete without kill-ring & inserting over selection.
(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame
(setq split-width-threshold nil) ;; Split window vertically by default
;; https://stackoverflow.com/questions/7997590/how-to-change-the-default-split-screen-direction


;; --- Disable all version control
;; since Emacs gets terribly slow
;; http://shallowsky.com/blog/linux/editors/no-emacs-version-control.html
(setq vc-handled-backends nil)

;; *** FILE:  column-and-line-numbers.el
;; --- Columns, line-numbers, etc.
(column-number-mode t)
(global-linum-mode t) ;; line numbers in all buffers


;; *** FILE:  paren-indent.el
;; --- Paren stuff
(show-paren-mode 1)
(electric-pair-mode 1)
(defvar electric-pair-pairs) ;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '( (?\" . ?\") (?\{ . ?\}) ) )
(electric-indent-mode 1) ;; indent after enter

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %.  ARG."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;; *** FILE:  cond-mac-linux-win.el
(cond
 ;; --- Mac OS X stuff ---
 ((string-equal system-type "darwin")
  (message "System: Mac")
  (setq mac-option-modifier 'command)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "M-w") 'kill-this-buffer) ;; this works on Mac too
  (define-key global-map (kbd "C-<f2>")
    (lambda ()
      (interactive)
      (x-popup-menu (list '(0 0) (selected-frame))
                    (mouse-menu-bar-map))))
  (set-face-attribute 'default nil :height 200))

 ;; --- Windows stuff ---
 ((string-equal system-type "windows-nt")
  (message "System: Windows")
  (set-face-attribute 'default nil :family "Consolas" :height 140)
  (add-to-list 'exec-path "%HOME%/scoop/apps/git/current/usr/bin")
  (setenv "PATH" (mapconcat #'identity exec-path path-separator))

  (add-to-list 'exec-path "c:/Users/etomort/hunspell/bin/")
  (setq ispell-program-name (locate-file "hunspell"
                                         exec-path exec-suffixes 'file-executable-p))
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default default-buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (setq find-program "%HOME%/scoop/apps/git/current/usr/bin/find.exe")
  (setq compile-command "build.bat")
  )
 
 ;; --- Linux stuff ---
 ((message "System: Linux")
  (set-face-attribute 'default nil :family "Consolas" :height 140)

  ;; --- Persistent sessions
  ;; https://github.com/thierryvolpiatto/psession
  ;; https://github.com/emacs-helm/helm/issues/2028
  ;;  (psession-mode 1)

  ;; This makes Emacs on Windows unusable...
  ;; So set it only on Linux
  ;; https://github.com/emacs-helm/helm/issues/1976
  (setq x-wait-for-event-timeout nil)
  )
 )

;; *** FILE:  duplicate-line.el
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


;; *** FILE:  xah-cut-copy.el
;; --- Cut, Copy, Paste from Xah Lee functions   ---
;; Check http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html
(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))


(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2017-07-08"
  (interactive)
    (if current-prefix-arg
      (progn
        (kill-ring-save (point-min) (point-max))
        (message "All visible buffer text copied"))
      (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end))
          (message "Active region copied"))
    (if (eq last-command this-command)
          (if (eobp)
              (progn (message "empty line at end of buffer." ))
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (message "Line copy appended")
        (progn
            (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10 )
                (progn (message "empty line at end of buffer." ))
              (progn
                (kill-ring-save (line-beginning-position) (line-end-position))
                (end-of-line)
                (message "line copied")))
      (progn
            (kill-ring-save (line-beginning-position) (line-end-position))
    (end-of-line)
    (forward-char)
            (message "line copied")))))))

;; *** FILE:  defalias.el
;; defalias for Fast M-x
;; http://ergoemacs.org/emacs/emacs_alias.html

(defalias 'hgrep 'helm-grep-do-git-grep)
(defalias 'hfind 'helm-find)
(defalias 'hman 'helm-man-woman)
(defalias 'hoccur 'helm-occur)
(defalias 'hr 'helm-recentf)
(defalias 'hrec 'helm-recentf)

;; TRY helm-swoop & swiper more...

(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

(defalias 'lb 'list-buffers)
(defalias 'lp 'list-processes)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'db 'ediff-buffers)
(defalias 'difbuf 'ediff-buffers)
(defalias 'diffil 'ediff-files)

(defalias 'trf 'transpose-frame)
(defalias 'trframe 'transpose-frame)
(defalias 'df 'delete-frame)
(defalias 'nf 'new-frame)

(defalias 'odired 'open-in-dired)

(defalias 'open-in-chrome 'browse-url-of-file)
(defalias 'oichrome 'browse-url-of-file)

(defun start-windows-explorer () (interactive) (shell-command "explorer.exe ."))
(defalias 'wx 'start-windows-explorer)
(defalias 'wexp 'start-windows-explorer)

;; *** FILE:  keybindings.el
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
(define-key input-decode-map [?\C-m] [C-m])
(global-set-key (kbd "<C-m>") 'kmacro-start-macro)
(global-set-key (kbd "<C-S-m>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "<C-f9>") 'kmacro-call-macro)


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
(global-set-key "%" 'match-paren) ;; Like vim
(global-set-key (kbd "C-M-j") 'down-list) ;; As C-M-u does backward-up-list


;; --- FX keys
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'mark-whole-buffer)
(global-set-key (kbd "<f7>") 'neotree-toggle)
(global-set-key (kbd "<f8>") 'ibuffer)

;; F9 & F12 are defined in compilation.el
(global-set-key (kbd "<f11>") 'indent-buffer)
(global-set-key (kbd "C-<f12>") 'start-windows-explorer)


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
(global-set-key (kbd "C-w") 'kill-this-buffer) ;; Just like Chrome, etc..
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

;; No me funciona... ya que no me deja meter por lo que quiero sustituir...
(defun query-replace-symbol-at-point ()
  "Start `query-replace-regexp' with symbol at point as default."
  (interactive)
  (let ((sym (symbol-at-point)))
    (when sym
      ;; (push (cons (format "\\_<%s\\_>" sym) "") query-replace-defaults)
      (push (cons (format "%s" sym) "") query-replace-defaults)
      (call-interactively #'query-replace))))


(global-set-key (kbd "C-S-r") 'query-replace) ;; Seems to remind me r=replace
(global-set-key (kbd "C-.") 'repeat) ;; Like . in vim
(global-set-key (kbd "M-r") 'iedit-mode)

(global-set-key (kbd "M-y") 'company-complete)
(global-set-key (kbd "M-;") 'hippie-expand)
(global-set-key (kbd "C-;") 'company-files)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "M-z") 'recenter-top-bottom)

(global-set-key (kbd "M-<f4>") 'kill-emacs)
(global-set-key (kbd "<escape>") 'keyboard-quit)

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

;; *** FILE:  basic-special.el
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "H-i") 'switch-to-buffer)

(eval-after-load "dired" '(progn
                            (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
                            (put 'dired-find-alternate-file 'disabled nil)
                            (define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
                            (define-key dired-mode-map (kbd "C-o") 'find-file)
                            (define-key dired-mode-map (kbd "C-w") 'kill-this-buffer)
                            (setq dired-listing-switches "-lkt")
                            )
                 )
(eval-after-load "ibuffer" '(progn
                              (define-key ibuffer-mode-map (kbd "C-o") nil)
                              (define-key ibuffer-mode-map (kbd "C-i") nil)
                              (define-key ibuffer-mode-map (kbd "M-h") 'toggle-ibuffer-groups)
                              (define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group)
                              )
                 )

;; (load "~/.emacs.d/elpa/hl-defined-20170223.744/hl-defined-autoloads.el")
;; (load "~/.emacs.d/elpa/wrap-region-20140117.720/wrap-region-autoloads.el")
;; (load "~/.emacs.d/elpa/company-20190116.1133/company-autoloads.el")
;; (load "~/.emacs.d/elpa/helm-20190213.609/helm-autoloads.el")

;; *** FILE:  cua.el
(defun special-c-return-in-dired ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (dired-w32explore)
    (cua-set-rectangle-mark))
  )

(define-key cua-global-keymap [C-return] 'special-c-return-in-dired)

