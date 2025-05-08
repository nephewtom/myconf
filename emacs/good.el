;; --- Packages stuff
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/packages")
(package-initialize)

;; --- Start-up some stuff

;; Provide timestamp to *Messages* logs
(load "~/myconf/emacs/log.el")

;; Backup files: https://www.johndcook.com/blog/emacs_windows/#backup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Try to speed up start-up
(setq-default gc-cons-threshold (* 100 1024 1024))

;; Follow git symlinks
(setq vc-follow-symlinks t)

(setq ring-bell-function
      (lambda ()
        (play-sound-file "~/myconf/emacs/hit.wav")))

(server-start)


;; --- Bars, title and theme
(setq inhibit-startup-message t)
(tool-bar-mode -1) ;; removes tool-bar
(menu-bar-mode -1) ;; removes tool-bar
(scroll-bar-mode t)
(setq default-directory "~/")
(setq frame-title-format '("tom@" (:eval (format "%s" system-type))
                           ": "(:eval (if (buffer-file-name)
                                          (buffer-file-name) "%b"))))

(fringe-mode '(16 . 0)) ;; Make left fringe 16 pixels and no right fringe


(load-theme 'tango-dark t)

;; https://stackoverflow.com/q/9990370/316232
(global-hl-line-mode t) ;; highlight current line
(make-variable-buffer-local 'global-hl-line-mode)

;; current line highlighted color
(set-face-background hl-line-face "#406040")
(set-face-foreground hl-line-face "#ffffff")
;; (set-face-attribute 'hl-line nil :inherit 'highlight :background nil)


;; region highlight color
(set-face-attribute 'region nil :background "#848000") ;;

;; fringe color (between line numbers and buffer)
(set-face-attribute 'fringe nil :background "#505050")


(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "forest green" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "forest green")))))




;; --- Miscellaneous
(setq set-mark-command-repeat-pop t) ;; https://emacs.stackexchange.com/a/2818/6957
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(setq-default tab-width 4)


(delete-selection-mode 1) ;; Allows to delete without kill-ring & inserting over selection.
(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame

;; https://stackoverflow.com/questions/7997590/how-to-change-the-default-split-screen-direction
(setq split-width-threshold 200) ;; For split window vertically 
;; Only this made it work for Tom


;; --- Disable all version control
;; since Emacs gets terribly slow
;; http://shallowsky.com/blog/linux/editors/no-emacs-version-control.html
(setq vc-handled-backends nil)


;; --- Columns, line-numbers, etc.
(column-number-mode t)
(global-display-line-numbers-mode)

;; --- Date format and forcing to use it in dired
;; (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
;; (setq ls-lisp-use-localized-time-format nil)
;; (setq ls-lisp-use-internal 'ls-lisp) ; Forces internal Emacs listing


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


(cond
 ;; --- Mac OS X stuff ---
 ((string-equal system-type "darwin")
  (message "System: Mac")
  (setq mac-option-modifier 'control)
  (setq mac-command-modifier 'meta)

  (global-set-key (kbd "M-w") 'kill-this-buffer) ;; this works on Mac too
  (define-key global-map (kbd "C-<f2>")
              (lambda () (interactive)
                (x-popup-menu (list '(0 0) (selected-frame)) (mouse-menu-bar-map))))
  
  (set-face-attribute 'default nil :height 190)
  (global-set-key (kbd "<f1> 7") 'browse-url-at-point)
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  (defun start-file-manager () (interactive) (shell-command "open ."))
  (setq explicit-shell-file-name "/bin/zsh")
  (setq shell-file-name "/bin/zsh")
  )

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
  (defun start-file-manager () (interactive) (start-process "fpilot-process" nil "fpilot.exe" "."))
  )
 
 ;; --- Linux stuff ---
 ((message "System: Linux")
  (set-face-attribute 'default nil :family "Liberation Mono-14" :height 120)
  (set-frame-font "Liberation Mono-14:antialias=1")

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


;; --- Move end of line / Join line

(defun move-end-of-line-newline-and-indent ()
  "Insert a newline, then indent according to major mode."
  (interactive "*")
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

;;; It allows you to move the current line using M-up / M-down
;; If a region is marked, it will move the region instead.
(require 'move-text)
(move-text-default-bindings)


;; ===== Main Keybindings =======
(require 'iso-transl) ;; Make dead keys work

;; NOTE: Do not bind C-y & M-w to anything.
;; NOTE: That way I keep their original function, in case I need it...
;; NOTE: M-w (kill-ring-save)
;; NOTE: C-y (yank) or (cua-paste)

;; --- Cua mode 
(cua-mode t) ;; Ctrl+Z, Ctrl+X, Ctrl+C, Ctrl+V (Cmd+ in Mac OSX)

(defun special-c-return-in-dired ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (dired-w32explore)
    (cua-set-rectangle-mark))
  )

(define-key cua-global-keymap [C-return] 'special-c-return-in-dired)



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
;; (global-set-key (kbd "<f7>") 'neotree-toggle)
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

;; Default query-replace as alias
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

(defalias 'er 'eval-region)
(defalias 'lp 'list-processes)

;; Frame stuff
(defalias 'trf 'transpose-frame)
(defalias 'trframe 'transpose-frame)
(defalias 'df 'delete-frame)
(defalias 'nf 'new-frame)

;; anzu will override this, search 'anzu' below
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



;; --- Elisp related

;; (require 'hl-defined)

;; (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)

;; http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)




;; --- Multiple Cursors

;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)

(global-set-key (kbd "C-:") 'mc/edit-lines)
(global-set-key (kbd "M-;") 'mc/mark-next-like-this)
(global-set-key (kbd "M-/") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-?") 'mc/mark-all-like-this)

(defun my/disable-c-electric-brace ()
  "Disable `c-electric-brace` in multiple-cursors mode."
  (local-set-key "{" 'self-insert-command)
  (local-set-key "}" 'self-insert-command))

(defun my/enable-c-electric-brace ()
  "Restore `c-electric-brace` when multiple-cursors mode is disabled."
  (local-set-key "{" 'c-electric-brace)
  (local-set-key "}" 'c-electric-brace))

(add-hook 'multiple-cursors-mode-hook
          (lambda ()
            (if multiple-cursors-mode
                (my/disable-c-electric-brace)
              (my/enable-c-electric-brace))))



;; --- smex - simpler and faster than helm
(require 'smex)
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(ido-mode 1)
(ido-everywhere 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)



;; --- Dired ---
(require 'dired)

;; Evitar el mensaje de confirmación para usar 'dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; Auto-refrescar dired si el contenido del directorio cambia
(setq dired-auto-revert-buffer t)

;; Mostrar detalles por defecto
(setq diredp-hide-details-initially-flag nil)

;; Activar auto-revert en dired
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Vincular teclas dentro de dired-mode
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "M-p") 'backward-paragraph)
  (define-key dired-mode-map (kbd "F") 'find-name-dired)
  (define-key dired-mode-map (kbd "j") 'dired-find-file)
  (define-key dired-mode-map (kbd "e") 'ora-ediff-files)
  (define-key dired-mode-map (kbd "P") 'peep-dired)
  (define-key dired-mode-map (kbd "h") 'toggle-hidden-dirs)
  (define-key dired-mode-map (kbd "<M-return>") 'dired-w32-browser)
  (define-key dired-mode-map (kbd "M-i") 'switch-to-buffer-other-window)
  (define-key dired-mode-map (kbd "<f2>") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))

  ;; Desvincular teclas
  (define-key dired-mode-map (kbd "C-o") nil)
  (define-key dired-mode-map (kbd "C-w") nil)
  (define-key dired-mode-map (kbd "M-i") nil))


;; --- Buffers stuff

(require 'buffer-move)
(defun win-swap () "Swap windows using buffer-move.el" (interactive)
       (if (null (windmove-find-other-window 'right))
           (buf-move-left)
         (buf-move-right)))
(global-set-key (kbd "C-2") 'win-swap)

;; To be used with defalias lb to open list-buffers in same window
(defun my-list-buffers (&optional arg)
  "Display a list of existing buffers in the current window."
  (interactive "P")
  (switch-to-buffer (list-buffers-noselect arg)))
(defalias 'lb 'my-list-buffers)

;; --- Uniquify 
(require 'uniquify)
;;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
;;(toggle-uniquify-buffer-names) ;; Different buffer name for same name files


;; --- Ibuffer ----
(require 'ibuffer)

(define-key ibuffer-mode-map (kbd "C-o") nil)
(define-key ibuffer-mode-map (kbd "C-i") nil)
  ;; (define-key ibuffer-mode-map (kbd "M-h") 'toggle-ibuffer-groups)
(define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group)


;; how-can-i-make-ibuffer-auto-refresh-the-list-of-buffers
;; https://emacs.stackexchange.com/a/2179/6957
(defun my-ibuffer-stale-p (&optional noconfirm)
  ;; let's reuse the variable that's used for 'ibuffer-auto-mode
  (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))

(defun my-ibuffer-auto-revert-setup ()
  (set (make-local-variable 'buffer-stale-function)
       'my-ibuffer-stale-p)
  (set (make-local-variable 'auto-revert-verbose) nil)
  (auto-revert-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-auto-revert-setup)


;; --- Ibuffer extension ---
(require 'ibuf-ext)
  (add-to-list 'ibuffer-never-show-predicates "^\\*")
  (add-to-list 'ibuffer-never-show-predicates "^ \\*")

;; From: http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html

(defun my-ibuffer-saved-groups ()
  (setq ibuffer-saved-filter-groups
        '(("home"
           ("myconf" (or (filename . "myconf")
                         (filename . "emacs-config")))
           ("SMIP" (filename . "smip"))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("playground" (filename . "playground"))
           ("/usr/include/" (filename . "/usr/include/"))
           ("SDL" (filename . "tomas/SDL/"))
           ("raylib" (filename . "raylib"))
           )
          ("default")
          ))
  (ibuffer-switch-to-saved-filter-groups "home" ))


(add-hook 'ibuffer-mode-hook 'my-ibuffer-saved-groups)


(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

;; Ibuffer formats like column width
(define-ibuffer-column vtime
  (:name "Time" :inline t)
  (string ?a ?b ?c ?d)
  )

(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 30 30 :left :elide) ; change: 30s were originally 18s
              " "

              ;; TODO
              ;;              (vtime 4 4 :left)
              ;;              " "
              (mode 16 16 :left :elide)
              " "
              (size 9 -1 :right)

              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(defalias 'db 'ediff-buffers)
(defalias 'difbuf 'ediff-buffers)
(defalias 'eb 'eval-buffer)
(defalias 'ib 'indent-buffer)


;; --- Company ---
(require 'company)

;; Enable company globally
(global-company-mode 1)

;; Also enable company-mode for Emacs Lisp buffers
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Configuration
(setq company-idle-delay 0)
(setq company-global-modes '(not processing-mode text-mode))

;; Add dabbrev backend (useful for header files and generic completions)
(add-to-list 'company-backends 'company-dabbrev)

;; Key bindings
(define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; Optional: Bind helm-company (if you're using Helm)
(global-set-key (kbd "M-y") 'helm-company)


 ;; --- Mac OS X specific kill-word() ---
(cond
 ((string-equal system-type "darwin")

  ;; Avoid C-k and M-backspace overwrite clipboard
  (defun my-backward-kill-word ()
    "Kill the previous word without overwriting the clipboard."
    (interactive)
    (let ((clipboard-content (gui-get-selection 'CLIPBOARD))) ;; Save clipboard
      (backward-kill-word 1)
      (gui-set-selection 'CLIPBOARD clipboard-content))) ;; Restore clipboard
  
  (global-set-key (kbd "M-<backspace>") 'my-backward-kill-word)

  (defun my-kill-line ()
    "Kill the line without overwriting the clipboard."
    (interactive)
    (let ((clipboard-content (gui-get-selection 'CLIPBOARD))) ;; Save clipboard
      (kill-line)
      (gui-set-selection 'CLIPBOARD clipboard-content))) ;; Restore clipboard
  
  (global-set-key (kbd "C-k") 'my-kill-line)
  )
 )


(require 'anzu)
(global-anzu-mode +1)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key (kbd "M-%") 'anzu-query-replace-at-cursor)


;; --- Cpp with Lsp-mode, but not by default (use only when needed)

;; --- aggressive-indent
(require 'aggressive-indent)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(aggressive-indent-global-mode)

;; --- lsp-mode
(require 'lsp-mode)

;; Enable lsp in specific modes
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

;; Enable which-key integration
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

;; LSP settings
(setq lsp-diagnostics-provider :flycheck)
(setq lsp-clients-clangd-executable "clangd")
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp/insert-text-mode-adjust-indentation 4)
(setq lsp-completion-provider :capf)

;; Disable eldoc in lsp
(add-hook 'lsp-mode-hook (lambda () (eldoc-mode -1)))

;; Flycheck bindings
(add-hook 'lsp-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<f9>") 'flycheck-list-errors)
            (local-set-key (kbd "M-<f5>") 'flycheck-next-error)
            (local-set-key (kbd "M-<f6>") 'flycheck-previous-error)))

;; LSP keybindings
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "RET") #'my-lsp-newline)
  (define-key lsp-mode-map (kbd "M-RET") #'lsp-execute-code-action)
  (when (boundp 'lsp-signature-mode-map)
    (unbind-key "M-n" lsp-signature-mode-map)
    (unbind-key "M-p" lsp-signature-mode-map)))

(put 'lsp-clients-clangd-args 'safe-local-variable #'listp)

(set-face-attribute 'lsp-face-highlight-textual nil :background "#505000")
(set-face-attribute 'lsp-face-highlight-textual nil :foreground "#ffffff")


;; --- cc-mode
(require 'cc-mode)
(setq c-default-style "linux")
(setq c-basic-offset 4)

(defun my/cpp-mode-keybindings ()
  (local-set-key (kbd "C-c o") #'ff-find-other-file))

(add-hook 'c-mode-hook #'my/cpp-mode-keybindings)
(add-hook 'c++-mode-hook #'my/cpp-mode-keybindings)

(add-hook 'c-mode-hook (lambda () (eldoc-mode -1)))
(add-hook 'c++-mode-hook (lambda () (eldoc-mode -1)))

(with-eval-after-load 'cc-mode
  (unbind-key "C-M-h" c++-mode-map)
  (unbind-key "C-M-j" c++-mode-map))

;; --- glsl-mode
(require 'glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; Optional line numbers, indentation, pairing (commented out)
;; (add-hook 'glsl-mode-hook #'display-line-numbers-mode)
;; (add-hook 'glsl-mode-hook #'electric-indent-mode)
;; (add-hook 'glsl-mode-hook #'electric-pair-mode)

;; TODO/FIXME highlights
(require 'hl-todo)
(add-hook 'glsl-mode-hook #'hl-todo-mode)

;; GLSL viewer command
(defun run-glsl-viewer ()
  "Run glslViewer on the current GLSL shader file."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (start-process "glslViewer.exe" "*glslViewer*" "glslViewer" buffer-file-name)))

(with-eval-after-load 'glsl-mode
  (define-key glsl-mode-map (kbd "C-<f4>") #'run-glsl-viewer))



;; --- Importing Tsoding simpc-mode
(add-to-list 'load-path "~/myconf/emacs")
(require 'simpc-mode)
;; Automatically enabling simpc-mode on files with extensions like .h, .c, .cpp, .hpp
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

