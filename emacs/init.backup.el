
;; *** FILE:  begin.el
;; Provide timestamp to *Messages* logs
(load "~/myconf/emacs/log.el")
(message "Emacs BEGIN")

(setq custom-file "~/myconf/emacs/custom.el")

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

(setq ring-bell-function
      (lambda ()
        (play-sound-file "~/myconf/emacs/hit.wav")))

(server-start)

;; *** FILE:  package.el
;; Tell Emacs where to save compiled files
;; (when (boundp 'native-comp-eln-load-path)
;;   (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; activate all the packages (in particular autoloads)
(package-initialize)

(setq package-check-signature nil)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; list the packages you want
(setq package-list '(use-package diminish edit-server buffer-move monokai-theme))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

;; set the path for manually installed packages
(add-to-list 'load-path "~/.emacs.d/packages")

(setq native-comp-async-report-warnings-errors nil)

;; *** FILE:  theme.el
(load-theme 'monokai t)

;; https://stackoverflow.com/q/9990370/316232
(global-hl-line-mode t) ;; highlight current line
(make-variable-buffer-local 'global-hl-line-mode)

;; current line highlighted color
(set-face-background hl-line-face "#404040")

;; region highlight color
(set-face-attribute 'region nil :background "#848000") ;;

;; fringe color (franja a la izquierda del buffer)
(set-face-attribute 'fringe nil :background "#505050")

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "forest green" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "forest green")))))

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

;; *** FILE:  column-and-line-numbers.el
;; --- Columns, line-numbers, etc.
(column-number-mode t)
(global-display-line-numbers-mode)

;; --- Date format and forcing to use it in dired
;; (setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
;; (setq ls-lisp-use-localized-time-format nil)
;; (setq ls-lisp-use-internal 'ls-lisp) ; Forces internal Emacs listing

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


;; *** FILE:  calendar-conf.el
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


;; *** FILE:  cond-mac-linux-win.el
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

;; *** FILE:  sudo.el
;; From: http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

(defalias 'lb 'my-list-buffers)
(defalias 'db 'ediff-buffers)
(defalias 'difbuf 'ediff-buffers)
(defalias 'eb 'eval-buffer)
(defalias 'ib 'indent-buffer)

(defalias 'er 'eval-region)
(defalias 'lp 'list-processes)

(defalias 'trf 'transpose-frame)
(defalias 'trframe 'transpose-frame)
(defalias 'df 'delete-frame)

(defalias 'odired 'open-in-dired)
(defalias 'diffil 'ediff-files)

(defalias 'open-in-chrome 'browse-url-of-file)
(defalias 'oichrome 'browse-url-of-file)

;; *** FILE:  dired-conf.el
;; --- Dired ---

;; TODO: Sort dired by time date as default 
;; https://superuser.com/questions/875241/emacs-dired-sorting-by-time-date-as-default
;; (defvar my-dired-listing-switches nil)
;; (defvar my-dired-listing-switches-flag t)
;; (defun toggle-hidden-dirs ()
;;   (interactive)
;;   (if my-dired-listing-switches-flag
;;       (dired-sort-toggle-or-edit "-lkta --time-style=long-iso")
;;     (dired-sort-toggle-or-edit "-lkt --time-style=long-iso"))
;;   (setq my-dired-listing-switches-flag (not my-dired-listing-switches-flag))
;;   (dired-sort-toggle-or-edit))

(use-package dired
  :bind (:map dired-mode-map
              ("f" . dired-find-alternate-file)
              ("M-p" . backward-paragraph)
              ("F" . find-name-dired)
              ("j" . dired-find-file)
              ("e" . ora-ediff-files)
              ("P" . peep-dired)
              ("h" . toggle-hidden-dirs)
              ("<M-return>" . dired-w32-browser)
              ("M-i" . switch-to-buffer-other-window)
              ("<f2>" . wdired-change-to-wdired-mode)
              )
  
  :config
  (define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
  (put 'dired-find-alternate-file 'disabled nil)  

  ;; Auto-refresh dired on file change
  (setq dired-auto-revert-buffer t)

  (setq diredp-hide-details-initially-flag nil)
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  ;; (require 'dired+)
  ;; (require 'dired-x)
  ;; (setq dired-omit-files "^\\...+$") ; Only omit hidden files

  (require 'bind-key)
  (unbind-key "C-o" dired-mode-map)
  (unbind-key "C-w" dired-mode-map)
  (unbind-key "M-i" dired-mode-map)


  (cond
   ((string-equal system-type "darwin")
    ;; Mac stuff
    )

   ((string-equal system-type "windows-nt")
    ;; Windows stuff
    (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "AcroRd32.exe")
                                         ("\\.doc\\'" "WINWORD.EXE")
                                         ("\\.docx\\'" "WINWORD")
                                         ("\\.ppt\\'" "POWERPNT.EXE")
                                         ("\\.pptx\\'" "POWERPNT.EXE")
                                         ("\\.xls\\'" "EXCEL.EXE")
                                         ("\\.xlsx\\'" "EXCEL.EXE")
                                         ("\\.jpg\\'" "mspaint.exe")
                                         ("\\.png\\'" "mspaint.exe")
                                         ("\\.java\\'" "idea")))
    ) 
   (;; Ubuntu stuff
    (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "evince")
                                         ("\\.doc\\'" "libreoffice")
                                         ("\\.docx\\'" "libreoffice")
                                         ("\\.ppt\\'" "libreoffice")
                                         ("\\.pptx\\'" "libreoffice")
                                         ("\\.xls\\'" "libreoffice")
                                         ("\\.xlsx\\'" "libreoffice")
                                         ("\\.jpg\\'" "pinta")
                                         ("\\.png\\'" "pinta")
                                         ("\\.java\\'" "idea")))
    )
   )



  )

;; This does not seem to be loaded... Why?
;; (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "evince")
;;                                      ("\\.doc\\'" "libreoffice")
;;                                      ("\\.docx\\'" "libreoffice")
;;                                      ("\\.ppt\\'" "libreoffice")
;;                                      ("\\.pptx\\'" "libreoffice")
;;                                      ("\\.xls\\'" "libreoffice")
;;                                      ("\\.xlsx\\'" "libreoffice")
;;                                      ("\\.jpg\\'" "pinta")
;;                                      ("\\.png\\'" "pinta")
;;                                      ("\\.java\\'" "idea")))

(use-package peep-dired
  :ensure t)

;; Esta función usa dired-jump para abrir en dired el buffer actual
;; O a través de ibuffer
(defun open-in-dired ()
  "Show current buffer on dired."
  (interactive)
  (if (equal major-mode 'ibuffer-mode)
      (ibuffer-visit-buffer-in-dired)
    (if (buffer-file-name)
        (dired-jump nil (buffer-file-name))
      (message "This buffer is not a file in the filesystem."))))

(defun ibuffer-visit-buffer-in-dired (&optional noselect)
  "Visit the buffer on this line in dired."
  (interactive)
  (let ((buf (ibuffer-current-buffer t)))
    (bury-buffer (current-buffer))
    (dired-jump nil (buffer-file-name buf))
    (message (buffer-file-name buf))))



(setq find-args "")
(defun find-dired-by-date (dir args)
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		             (read-string "Run find (with args): " find-args
				                  '(find-args-history . 1))))
  ;; Set to this value in order to get a find sorted by date
  (setq find-ls-option '("-exec ls -lt {} + | cut -d ' ' -f5-" . "-lt"))
  (find-dired dir args)
  (setq find-ls-option '("-ls" . "-dilsb")))

(defun find-dired-by-size (dir args)
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		             (read-string "Run find (with args): " find-args
				                  '(find-args-history . 1))))
  ;; Set to this one to get it sorted by size
  (setq find-ls-option '("-exec ls -lSr {} + | cut -d ' ' -f5-" . "-lSr"))
  (find-dired dir args)
  (setq find-ls-option '("-ls" . "-dilsb")))


;; *** FILE:  ediff.el
;; https://oremacs.com/2017/03/18/dired-ediff/
;; -*- lexical-binding: t -*-
(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

;; From: https://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")

(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'ora-ediff-hook)

;; To restore windows configuration
;; https://emacs.stackexchange.com/a/17089/6957
(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

;; *** FILE:  helm-conf.el
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

;; *** FILE:  movement.el
;; --- Move end of line / Join line
(defun move-end-of-line-newline-and-indent ()
  "Insert a newline, then indent according to major mode."
  (interactive "*")
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

;;; It allows you to move the current line using M-up / M-down
;; If a region is marked, it will move the region instead.
(use-package move-text
  :ensure t
  :config (move-text-default-bindings))



(cond
 ;; --- Mac OS X specific ---
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

;; *** FILE:  buffers-utils.el
;; TODO: change by use-package
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

;; --- Uniquify 
(require 'uniquify)
;;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
;;(toggle-uniquify-buffer-names) ;; Different buffer name for same name files


;; --- Ibuffer ----
(use-package ibuffer
  :config 
  (define-key ibuffer-mode-map (kbd "C-o") nil) ;; unbind from default
  (define-key ibuffer-mode-map (kbd "C-i") nil) ;; unbind from default
  ;; (define-key ibuffer-mode-map (kbd "M-h") 'toggle-ibuffer-groups) ;; unbind from default
  (define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group) ;; unbind from default
  )


;; how-can-i-make-ibuffer-auto-refresh-the-list-of-buffers
;; Using this:
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
(use-package ibuf-ext
  :config 
  ;; Following lines eliminates annoying *Minibuf-, *Echo Area, etc., but also *magit: buffers
  ;; (add-to-list 'ibuffer-never-show-predicates " .*")

  ;; Remove the buffers that match these regexp
  (add-to-list 'ibuffer-never-show-predicates "^\\*")
  (add-to-list 'ibuffer-never-show-predicates "^ \\*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*grip-.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*NeoTree.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*lsp.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*Metahelp*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*tip*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*git-credential.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*Minibuf-.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*Echo Area.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*Custom.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*Python.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*DOC.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*SPP.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*temp.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*edit.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*ediff-tmp.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*emacs-query.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*autoload.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*spool.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*code-conver.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*helm.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*Deletions.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*http.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*RNC.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*elpy.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*server.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*org.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\*org.*")
  ;; (add-to-list 'ibuffer-never-show-predicates " .*\\Marked.*")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm ag")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm mini")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm find")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm grep exts")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm M-x")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm-mode")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm buffers")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*helm man woman*")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*Helm Swoop")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*Disabled")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*Help")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*tramp")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*JDEE")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*magit.*process")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*magit.*diff")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*magit.*log")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*canonical address.*")
  ;; (add-to-list 'ibuffer-never-show-predicates "^\\*extract address components.*")
  )

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

;; *** FILE:  elisp.el
;; --- Elisp related
;; (require 'hl-defined)

;; (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)

;; http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)


;; *** FILE:  flycheck-conf.el
(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; *** FILE:  company-conf.el
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; --- Company Mode ---
(use-package company
  :init
  (global-company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)

  :ensure t

  :config
  (company-mode)
  (setq company-idle-delay 0)
  (setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
  (add-to-list 'company-backends 'company-dabbrev) ;; Backend for header files
  

  :bind (:map company-search-map  
              ("C-t" . company-search-toggle-filtering)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(global-set-key (kbd "M-y") 'helm-company)


(defun helm-company-complete ()
  "Use helm to select company completions."
  (interactive)
  (when (company-manual-begin)
    (let ((helm-candidates (company-candidates)))
      (if helm-candidates
          (helm :sources (helm-build-sync-source "Company Completions"
                           :candidates helm-candidates
                           :action (lambda (candidate)
                                     (company-finish candidate)))
                :buffer "*helm-company*")
        (message "No completion candidates")))))

;; *** FILE:  google-translate.el
(use-package google-translate
  :ensure t
  :config
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "es")
  (global-set-key (kbd "C-S-<f8>") 'google-translate-at-point)
  (global-set-key (kbd "C-S-<f7>") 'google-translate-at-point-reverse)
  (require 'google-translate-default-ui)
  )

;; *** FILE:  org-mode.el
(use-package org
  :ensure
  :bind (("C-c l" . org-store-link)
         ("C-c o" . org-open-at-point)
         :map org-mode-map
         ("C-<tab>" . nil)
         ("M-h" . nil)                                  
         ("C-<down>" . org-forward-heading-same-level) 
         ("C-<up>" . org-backward-heading-same-level)  
         ("\C-ca" . org-agenda)                     
         ("C-c s" . org-edit-special)
         ("M-p" . backward-paragraph)
         ("M-n" . forward-paragraph)
         :map org-src-mode-map
         ("C-c s" . org-edit-src-exit))
  :config
  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "blue" :weight bold))
          ("IN-PROGRESS" . (:foreground "red" :weight bold))
          ("ON-HOLD" . (:foreground "orange" :weight bold))
          ("DONE" . (:foreground "forest green" :weight bold))

          ("TRY" . (:foreground "purple" :weight bold))
          ("NOTE" . (:foreground "black" :weight bold))
          ("REVIEW" . (:foreground "purple" :weight bold))
          ("PERMANENT" . (:foreground "purple" :weight bold))
          ("CANCELLED" . (:foreground "black" :weight bold))

          ("WTF" . (:foreground "orange" :weight bold)) ;; Color not working
          ))

  (require 'org-tempo)

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "ON-HOLD" "|" "DONE")
          (sequence "TRY" "NOTE" "REVIEW" "PERMANENT" "CANCELLED" "|" "WTF")))

  (setq org-priority-faces '((?A . (:background "#DD0000"  :foreground "black" :box '(:line-width 2 :style released-button)))
                             (?B . (:background "#A366FF" :foreground "black" :box '(:line-width 2 :style released-button)))
                             (?C . (:background "#00CCFF" :foreground "black" :box '(:line-width 2 :style released-button)))))

  (setq org-startup-truncated nil)
  
  (eval-after-load "org"
    '(require 'ox-md nil t))
  (eval-after-load "org"
    '(require 'ox-gfm nil t))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  )

(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;;(setq org-odt-category-map-alist
;; '(("__Figure__" "Illustration" "value" "Figure" org-odt--enumerable-image-p)))


(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)

(defalias 'org-open-link 'org-open-at-point)

;; TODO: To export the current org-file to markdown,
;; TODO: and then export that one to HTML and preview it in browser.
;; TODO: Not working... I am getting:  org-export-md-and-html-preview: Wrong number of arguments: (0 . 0), 1
;; TODO: Learn to debug emacs functions...
;;
;; (defun org-export-md-and-html-preview ()
;;   "Exports org-mode file to markdown, html and previews it in browser"
;;   (interactive)
;;   (markdown-export-and-preview (org-md-export-to-markdown)))

;; or...
;;  (markdown-export-and-preview (find-file '(org-md-export-to-markdown))))


;; Idea from:
;; (defun markdown-export-and-preview ()
;;   "Export to XHTML using `markdown-export' and browse the resulting file."
;;   (interactive)
;;   (browse-url-of-file (markdown-export)))

(defun org-toggle-link-display ()
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if org-descriptive-links
      (progn (org-remove-from-invisibility-spec '(org-link))
         (org-restart-font-lock)
         (setq org-descriptive-links nil))
    (progn (add-to-invisibility-spec '(org-link))
       (org-restart-font-lock)
       (setq org-descriptive-links t))))

;; *** FILE:  nxml.el
;; --- XML Stuff ---
;; (use-package sgml
;; ;  :ensure t
;;   :config
;;   (setq sgml-basic-offset 4)
;;   )

(use-package nxml-mode
                                        ;  :ensure t
  :mode (("\\.xml$" . nxml-mode)
         ("\\.xsd$" . nxml-mode)
         ("\\.sch$" . nxml-mode)
         ("\\.rng$" . nxml-mode)
         ("\\.xslt$" . nxml-mode)
         ;;         ("\\.svg$" . nxml-mode)
         ("\\.rss$" . nxml-mode)
         ("\\.wsdl$" . nxml-mode))

  :bind (:map nxml-mode-map
              ("C-c h" . hs-toggle-hiding))
  
  :config
  (setq nxml-child-indent 4 nxml-attribute-indent 4)
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

(use-package auto-complete-nxml
  :ensure t
  :config
  ;(setq auto-complete-nxml-popup-help-key "C-") ;; TODO: Change this key-binding
  (setq auto-complete-nxml-toggle-automatic-key "C-c C-t"))

;; From http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region (BEGIN, END).
You need to have 'nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))


;; To inform where are we in the XML
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun xml-find-file-hook ()
  (when (derived-mode-p 'nxml-mode)
    (which-function-mode t)
    (setq which-func-mode t)
    (add-hook 'which-func-functions 'nxml-where t t)))

(add-hook 'find-file-hook 'xml-find-file-hook t)

;; *** FILE:  markdown-conf.el
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.md.html$" . markdown-mode)
         ("\\.text$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))

  :bind ( ;;("C-M-m" . livedown:preview)
         :map markdown-mode-map
         ("C-c C-c t" . markdown-toc/generate-toc)
         ("M-p" . nil)
         ("M-n" . nil)
         ("C-<down>" . markdown-outline-next-same-level)
         ("C-<up>" . markdown-outline-previous-same-level))
 
  :config
  (setq markdown-open-command "markdownmonster.exe")
  ;;(setq markdown-command "markdown")
  )

;; IMP!: grip-mode is the way to go for Markdown preview
;; Though it FAILS with images in WSL Emacs... WTF! Check if I can fix that.

;; Forget about impatient mode and flymd
;; https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time

(defun markdown-html-no-title (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Tom Cooking Markdown...</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;; Gives error...
;;(imp-set-user-filter 'markdown-html)

;; (setq imp-user-filter 'markdown-filter-for-impatient-mode)
;; (cl-incf imp-last-state)
;; (imp--notify-clients)

(defun markdown-preview-browser ()
  (interactive)
  (impatient-mode 1)
  (setq imp-user-filter #'markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients))

;; *** FILE:  cpp.el
(use-package aggressive-indent
  :ensure t
  :hook (prog-mode . aggressive-indent-mode)) ;; Enable in all programming modes

(aggressive-indent-global-mode)


(defun my/flycheck-mode-line-status-text ()
  "Custom Flycheck status with colors: yellow for warnings, red for errors."
  (let ((error-count (flycheck-count-errors flycheck-current-errors)))
    (let ((errors (or (cdr (assq 'error error-count)) 0))
          (warnings (or (cdr (assq 'warning error-count)) 0)))
      (cond
       ((> errors 0)
        (propertize (format " FlyC:%d|%d" errors warnings)
                    'face 'error))
       ((> warnings 0)
        (propertize (format " FlyC:%d|%d" errors warnings)
                    'face 'warning))
       (t
        (propertize " FlyC:OK" 'face 'success))))))

(setq flycheck-mode-line
      '(:eval (my/flycheck-mode-line-status-text)))

(defun my-lsp-newline ()
  "Custom function to override ENTER behavior in LSP buffers."
  (interactive)
  (insert-char ?\n)
  (indent-according-to-mode))

;; --- LSP Mode ---
(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         ;; (python-mode . lsp)
         ;; (js-mode . lsp)
         ;; Add more languages as needed, but NOT `emacs-lisp-mode`
         (lsp-mode . lsp-enable-which-key-integration)) ;; Show key hints

  :init
  (setq lsp-diagnostics-provider :flycheck)

  :bind (:map lsp-mode-map
              ("RET" . my-lsp-newline)
              ("M-RET" . lsp-execute-code-action))
  
  :config
  (setq lsp-clients-clangd-executable "clangd")
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp/insert-text-mode-adjust-indentation 4)

  ;; From https://ianyepan.github.io/posts/emacs-ide/
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  ;; (setq lsp-restart 'auto-restart) ;; commented by Tom
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil) ;; already set
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5)






  

  (unbind-key "M-n" lsp-signature-mode-map)
  (unbind-key "M-p" lsp-signature-mode-map)
  
  ;; TOM Testing
  (setq lsp-completion-provider :none)      ;; Disable LSP completion provider (CAPF or others)
  (setq lsp-completion-provider :capf)




  (add-hook 'lsp-mode-hook (lambda () (eldoc-mode -1)))

  (add-hook 'lsp-mode-hook
            (lambda ()
              (local-set-key (kbd "M-<f9>") 'flycheck-list-errors)
              (local-set-key (kbd "M-<f5>") 'flycheck-next-error)
              (local-set-key (kbd "M-<f6>") 'flycheck-previous-error)
              ))


  (put 'lsp-clients-clangd-args 'safe-local-variable #'listp)

  ;; (setq lsp-clients-clangd-args
  ;;       '(
  ;;         "--compile-commands-dir=D:/playground/raylib/trayimg/" ;; Set compile_commands.json location
  ;;         "--query-driver=C:/mingw-w64/x86_64-8.1.0-win32-seh-rt_v6-rev0/mingw64/bin/g++.exe"
  ;;         "--header-insertion=never"
  ;;         ))

  )


;; --- Eglot ---
;; (use-package eglot
;;   :ensure t
;;   :hook ((c-mode . eglot-ensure)
;;          (c++-mode . eglot-ensure))
;;   :config
;;   (setq eglot-autoshutdown t)  ;; Automatically shutdown eglot when the buffer is killed
;;   ;; Optional: Set clangd as the LSP server for C/C++
;;   (setq eglot-server-programs '((c++-mode . ("clangd"))
;;                                 (c-mode . ("clangd"))))

;;   (setq eglot-stay-out-of '(flymake))

;;   (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1))) ;; Disable Eldoc
;;   (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;;   )



(use-package cc-mode
  :config
  (unbind-key "C-M-h" c++-mode-map)
  (unbind-key "C-M-j" c++-mode-map)
  (setq c-default-style "linux") ;; BSD/Allman brackets
  (setq c-basic-offset 4)      ;; 4-space indent

  (defun my/cpp-mode-keybindings ()
    (local-set-key (kbd "C-c o") 'ff-find-other-file)) ;; swith header/impl
  (add-hook 'c-mode-hook #'my/cpp-mode-keybindings)
  (add-hook 'c++-mode-hook #'my/cpp-mode-keybindings)
  
  :hook ((c-mode . (lambda () (eldoc-mode -1)))
         (c++-mode . (lambda () (eldoc-mode -1))))
  )


(which-function-mode)

;;(add-hook 'c-mode-common-hook 'flycheck-color-mode-line-mode)

;; (Conditional) C/C++ Keybinds
;; (add-hook 'c-mode-common-hook
;; (lambda () (local-set-key (kbd "M-o") 'ff-find-other-file)))


(use-package glsl-mode
  :ensure t
  :mode (("\\.vs\\'" . glsl-mode)
         ("\\.fs\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode))
  :config
  ;; Enable line numbers for GLSL editing
  ;; (add-hook 'glsl-mode-hook #'display-line-numbers-mode)

  ;; Enable automatic indentation
  ;; (add-hook 'glsl-mode-hook #'electric-indent-mode)

  ;; Highlight TODO/FIXME comments
  (use-package hl-todo
    :ensure t
    :hook (glsl-mode . hl-todo-mode))

  ;; Enable automatic bracket pairing
  ;; (add-hook 'glsl-mode-hook #'electric-pair-mode)


  (defun run-glsl-viewer ()
    "Run glslViewer on the current GLSL shader file."
    (interactive)
    (when buffer-file-name
      (save-buffer)  ;; Save before running
      (start-process "glslViewer.exe" "*glslViewer*" "glslViewer" buffer-file-name)))

  ;; Ensure keybinding is only set after glsl-mode is loaded
  (with-eval-after-load 'glsl-mode
    (define-key glsl-mode-map (kbd "C-<f4>") 'run-glsl-viewer)))



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

;; *** FILE:  python-conf.el
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map  python-mode-map
               ("C->" . python-indent-shift-right)
               ("C-<" . python-indent-shift-left))
  :config
  ;; (elpy-enable)
  ;; What about these two if I use virtualenv ?
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")

  ;; (setq python-shell-interpreter "ipython3"
  ;;       python-shell-interpreter-args "-i --simple-prompt")

  
  ;; (setq elpy-rpc-python-command "python3")  
  ;; (setq elpy-rpc-backend "jedi") ;; Do I have to setq this?

  ;; (when (require 'flycheck nil t)
  ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; (setq flycheck-python-flake8-executable "python3")
  ;; (setq flycheck-python-pycompile-executable "python3")
  ;; (setq flycheck-python-pylint-executable "python3")
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)

  )
;; I think jedi uses autocomplete-mode (AC) and not company-mode
;;(add-hook 'python-mode-hook 'jedi:setu)
;;(setq jedi:complete-on-dot t)

;; (setq elpy-rpc-python-command "python2")  

;; *** FILE:  compilation.el
(global-set-key (kbd "M-<f12>") 'compile)

;; Hook for compilation buffer
(require 'ansi-color)
(defun my-ansi-color-apply ()
  "Apply ANSI colors to the *compilation* buffer."
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun my-compilation-hook ()
  "Ensure *compilation* splits vertically and applies ANSI colors."
  (unless (get-buffer-window "*compilation*")
    (split-window-vertically))

  (add-hook 'compilation-filter-hook 'my-ansi-color-apply))

(add-hook 'compilation-mode-hook 'my-compilation-hook)



;; Function for compiling 
(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn

    ;; Kill the running compilation process if it exists
    (let ((comp-proc (get-buffer-process "*compilation*")))
      (when comp-proc
        (sit-for 0.1)  ;; Give Emacs a short delay to properly kill the process
        (when (process-live-p comp-proc)  ;; If still running, force kill
          (delete-process comp-proc)
          )))

    ;; Now run recompile and force it to not prompt (avoid the "kill it?" prompt)
    (let ((compilation-ask-about-save nil))  ;; Prevent asking about saving buffers
      (call-interactively 'recompile))
    
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 15))
    (select-window cur)
    ))
(global-set-key (kbd "<f12>") 'my-compile)

;; Hide compilation buffer
(defun hide-compilation-buffer ()
  (interactive)
  (let ((w (get-buffer-window "*compilation*")))
    (when w
      (delete-window w))))
(global-set-key (kbd "C-<f12>") 'hide-compilation-buffer)


;; Change default comment for Windows .BAT files
(defun my-bat-mode-hook ()
  (setq comment-start ":: ")
  (setq comment-start-skip "::[ \t]*"))

(add-hook 'bat-mode-hook 'my-bat-mode-hook)


;; FROM HERE TILL THE END, NOT USED NOW!

;; Funtion to run compiled programs
(cond
 ((string-equal system-type "windows-nt")
  (message "System: Windows")
  (setq compile-command "build.bat")

  (defun run-program () (interactive)
         (when (get-buffer "*run*")
           (kill-buffer "*run*"))
         
         (when (get-buffer "*compilation*")
           (delete-window (get-buffer-window (get-buffer "*compilation*")))
           (kill-buffer "*compilation*"))
         (add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)) )
         (async-shell-command "run.bat")
         (switch-to-buffer (get-buffer "*Async Shell Command*"))
         (rename-buffer "*run*")
         (switch-to-previous-buffer)
         )
  
  (defun clean-program () (interactive)
         (when (get-buffer "*clean*")
           (kill-buffer "*clean*"))
         (add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)) )
         (async-shell-command "clean.bat")         
         (switch-to-buffer (get-buffer "*Async Shell Command*"))
         (rename-buffer "*clean*")
         (switch-to-previous-buffer)
         )
  
  ;; (global-set-key (kbd "<f9>") 'run-program)
  ;; (global-set-key (kbd "<f11>") 'clean-program)
  )

 ((message "System: Other"))
 )


;; Helper for compilation. Close the compilation window if there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))

;; Specify my function (maybe I should have done a lambda function)
;;(setq compilation-exit-message-function 'compilation-exit-autoclose)


;; *** FILE:  multiple-cursors.el
;; https://github.com/magnars/multiple-cursors.el

;; TO TEST THIS, USE THE FILE: multiple-cursors.playground

(use-package multiple-cursors
  :ensure t
  :bind (("C-:" . mc/edit-lines)           ;; Add cursor to each line in selection
         ("M-;" . mc/mark-next-like-this)  ;; Add cursor to next occurrence
         ("M-/" . mc/mark-previous-like-this) ;; Add cursor to previous occurrence
         ("M-?" . mc/mark-all-like-this))) ;; Add cursors to all occurrences


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

;; *** FILE:  cua.el
(defun special-c-return-in-dired ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (dired-w32explore)
    (cua-set-rectangle-mark))
  )

(define-key cua-global-keymap [C-return] 'special-c-return-in-dired)


;; *** FILE:  super-save-conf.el
(use-package super-save
  :ensure t
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 5)
  (super-save-mode 1)

  ;; Manually add an idle timer
  (run-with-idle-timer super-save-idle-duration t #'super-save-command))

;; *** FILE:  end.el
;; TODO: Print date in scratch buffer
;; (message (format-time-string "%H:%M:%S.%3N"))
;; (setq myscratch (get-buffer "*scratch*"))
;; (print (format-time-string "%H:%M:%S.%3N") myscratch)
;; (print "rubbish"  myscratch)

(put 'scroll-left 'disabled nil)


;;(load "~/myconf/emacs/rg.el")
;;(load "~/myconf/emacs/smart-line.el")
;;(load "~/myconf/emacs/spaceline.el")

(message "Emacs ready with init.el !")
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Problem when hitting Alt+Tab on Windows
;;(global-set-key (kbd "<Scroll_Lock>") '(lambda () (interactive) nil ))
(global-set-key (kbd "<Scroll_Lock>") 'ignore)

;; --- Recent files stuff
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(defalias 'rf 'recentf-open-files)
(defalias 'recf 'recentf-open-files)

;; --- Font-size & split-pane size
;; For some reason this get disabled if I put it in keybinding.el
(global-set-key (kbd "C-=") 'text-scale-adjust)


(load-file custom-file)
(require 'recentf)
(recentf-mode 1)
(recentf-open-files)


(defun profile-emacs-startup ()
  "Calculate and display the total time spent during Emacs startup based on log timestamps."
  (interactive)
  (with-current-buffer "*Messages*"
    (let ((begin-timestamp nil)
          (end-timestamp nil))
      ;; Find BEGIN timestamp
      (goto-char (point-min))
      (when (search-forward "Emacs BEGIN" nil t)
        (beginning-of-line)
        (when (looking-at "\\[\\(.*?\\)\\]")
          (setq begin-timestamp (match-string 1))))

      ;; Find END timestamp
      (goto-char (point-min))
      (when (search-forward "Emacs END" nil t)
        (beginning-of-line)
        (when (looking-at "\\[\\(.*?\\)\\]")
          (setq end-timestamp (match-string 1))))

      (if (and begin-timestamp end-timestamp)
          ;; (progn

          ;; Claude did this... TERRIBLE!
          ;; Parse timestamps manually - format is YYYY-MM-DDThh:mm:ss.ssssss
          (let* ((begin-time-parts (split-string begin-timestamp "[T:]"))
                 (end-time-parts (split-string end-timestamp "[T:]"))
                 (begin-seconds-parts (split-string (nth 3 begin-time-parts) "\\."))
                 (end-seconds-parts (split-string (nth 3 end-time-parts) "\\."))

                 (begin-hour (string-to-number (nth 1 begin-time-parts)))
                 (begin-min (string-to-number (nth 2 begin-time-parts)))
                 (begin-sec (string-to-number (nth 0 begin-seconds-parts)))
                 (begin-microsec (string-to-number (nth 1 begin-seconds-parts)))

                 (end-hour (string-to-number (nth 1 end-time-parts)))
                 (end-min (string-to-number (nth 2 end-time-parts)))
                 (end-sec (string-to-number (nth 0 end-seconds-parts)))
                 (end-microsec (string-to-number (nth 1 end-seconds-parts)))

                 (begin-total-secs (+ (* begin-hour 3600) (* begin-min 60) begin-sec (/ begin-microsec 1000000.0)))
                 (end-total-secs (+ (* end-hour 3600) (* end-min 60) end-sec (/ end-microsec 1000000.0)))
                 (diff-secs (- end-total-secs begin-total-secs)))

            (format "Emacs startup time: %.3f seconds" diff-secs))
        "Could not find BEGIN and END markers in startup log."))))

(message "Emacs END")
(message (profile-emacs-startup))

