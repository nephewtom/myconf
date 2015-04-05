;;; package --- Emacs init.el file
;;; Commentary:

;;; Code:
;;; General topics.
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
;; Reminder! To untabify a whole buffer, use: C-x h to mark it all
;; and then use: M-x untabify

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode t) ;; automatically revert buffer when file changes

;; changes default Emacs behaviour, allowing to delete without kill-ring & inserting over selection.
(delete-selection-mode 1)

;;; --- Personal rebinding of common keys

;; Similar to vim as they may remind me so...
(global-set-key (kbd "M-j") 'join-line) ;; Almost like J in vim (joins to previos line)
(global-set-key (kbd "M-z") 'recenter-top-bottom) ;; Almost as zz zt...
(global-set-key (kbd "C-.") 'repeat) ;; Like . in vim?
(defun yyank-like-vim ()
  "Emulates yy command on vim."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (move-beginning-of-line 1))
(global-set-key (kbd "C-y") 'yyank-like-vim)

;; These two move cursor down/up 10 characters. Personal taste.
(global-set-key "\M-n" "\C-u10\C-n")
(global-set-key "\M-p" "\C-u10\C-p")
(global-set-key (kbd "M-ç") 'delete-horizontal-space)
;; http://stackoverflow.com/questions/445225/emacs-command-to-delete-up-to-non-whitespace-character

(global-set-key (kbd "C-l") 'goto-line) ;; Like Eclipse
(global-set-key (kbd "C-S-r") 'query-replace) ;; Seems to remind me r=replace
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f11>") 'hc-toggle-highlight-tabs)
(global-set-key (kbd "M-ñ") 'eval-expression) ;; Same as US layout
(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame


;; --- Packages ELPA, MELPA, Marmalade ---
;; Needs to be before any package in those. E.g.: It fails to loas buffer-move,
;; if (require 'buffer-move) is placed just before this package stuff
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; --- Font size & Mac OS X stuff
(cond
 ((string-equal system-type "darwin")
  ;; Mac stuff
  (setq mac-option-modifier 'command)
  (setq mac-command-modifier 'meta)

  (define-key global-map (kbd "C-<f2>")
    (lambda ()
      (interactive)
      (x-popup-menu (list '(0 0) (selected-frame))
                    (mouse-menu-bar-map))))

  (set-face-attribute 'default nil :height 200))
 ;; Ubuntu stuff
 ((set-face-attribute 'default nil :height 140)))

;; Theme & Fonts
;;(load-theme 'wombat t)
;;(set-default-font "Ubuntu Mono 13")


;; --- Ido stuff ---
(require 'ido)
(ido-mode t)
(defvar ido-dont-ignore-buffer-names '("*scratch*" "*eshell*" "*shell*"))
(defun ido-ignore-most-star-buffers (name)
  "This function make ido ignore NAME buffers with star except those of previous variable."
  (and
   (string-match-p "^*" name)
   (not (member name ido-dont-ignore-buffer-names))))
(setq ido-ignore-buffers (list "\\` " #'ido-ignore-most-star-buffers))


;; --- Buffers & Ibuffer stuff ---
(global-set-key (kbd "C-w") 'kill-this-buffer) ;; Just like Chrome, etc..
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Prefer ibuffer to buffers-list
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1))) ;; Update ibuffer automatically

;; (defadvice split-window (after move-point-to-new-window activate)
;;   "Move d."
;;   (other-window 1)
;;   (switch-to-previous-buffer)
;;   (other-window 1))

(defun switch-to-previous-buffer ()
  "Swap to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "<f8>") 'switch-to-previous-buffer) ;; Personal taste

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows.  ARG?
buffer-move functions already provide this functionality.
Check buf-move-right, left, up, down"
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun indent-buffer ()
  "Select current buffer and indent it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "<f9>") 'indent-buffer) ;; Personal taste

(toggle-uniquify-buffer-names) ;; Different buffer name for same name files


;; --- Emacs windows stuff
(global-set-key [C-next] 'windmove-right)
(global-set-key [C-prior] 'windmove-left)
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] 'windmove-up)

(require 'buffer-move)
(defun win-swap ()
  "Swap windows using buffer-move."
  (interactive)
  (if (null (windmove-find-other-window 'right))
      (buf-move-left)
    (buf-move-right)))

;; useful library. Provides: transpose-frame,
;; flip-frame(Vertical), flop-frame(Horizontal),
;; rotate-frame (180º), rotate-frame-clockwise, rotate-frame-anticlockwise
(require 'transpose-frame)


;; --- Helm ---
(require 'helm)
(require 'helm-config)
(helm-autoresize-mode t) ;; do not know why does not work
(global-set-key (kbd "M-x") 'helm-M-x)
;; In case I need to change it.
;;(global-set-key (kbd "M-x") 'execute-extended-command)


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


;; --- Dired ---
(require 'dired )
(setq dired-listing-switches "-lk")
;; move to up directory with '.'
(define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
;; Following key is already binded to 'a'
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)


;; --- Elisp ---
(require 'hl-defined)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)


;; --- Auto-Complete & Yasnippet ---
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "C-M-y") 'auto-complete)

(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;;(define-key yas-minor-mode-map (kbd "C-y") 'yas-expand)
;; TODO: choose key... but first learn to use it...


;; --- Company, Irony, Clang, C++ stuff  ---
(require 'company)
(setq company-global-modes '(not emacs-lisp-mode processing-mode))
(global-set-key (kbd "M-y") 'company-complete)

(add-hook 'c-mode-common-hook 'my-after-c-hook)
(defun my-after-c-hook ()
  "This is my hook after c-mode-common-hook."
  (global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-clang)
  (add-to-list 'company-backends 'company-c-headers)
  (setq company-idle-delay 0)
  (setq c-basic-offset 4)
  )

;; From Irony https://github.com/Sarcasm/irony-mode .
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Switches between .h & .cpp files in C/C++
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "<f4>") 'ff-find-other-file) ;; like QtCreator

;; Disable AC mode in for c++
(defadvice auto-complete-mode (around disable-auto-complete-for-c++)
  "Disable AC (auto-complete mode) for c mode, as I use company."
  (unless (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)) ad-do-it))
(ad-activate 'auto-complete-mode)

;; (require 'google-c-style) ;; TODO: Not tested
;; (add-hook 'c-mode-common-hook 'google-set-c-style)

;; Considering _ part of a word
;; (modify-syntax-entry ?_ "w" c++-mode-syntax-table)


;; --- Flycheck / other language related keys ---
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "<f12>") 'recompile)
(global-set-key (kbd "<f3>") 'ffap)
(global-set-key (kbd "C-,") 'iedit-mode)


;; --- ggtags ---
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;; --- Markdown stuff ---
(add-to-list 'auto-mode-alist '("\\.wsdl\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-open-command "haroopad")

(eval-after-load 'markdown-mode '(define-key markdown-mode-map (kbd "C-c C-c t") 'markdown-toc/generate-toc))

(require 'edit-server)
(edit-server-start)
(add-to-list 'edit-server-url-major-mode-alist '("^stackoverflow" . markdown-mode))

(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))


;; --- Lua & Löve ---
(add-to-list 'load-path "~/.emacs.d/auto-complete-lua.el/")
(add-to-list 'load-path "~/.emacs.d/auto-complete-love.el/")
(require 'auto-complete-lua)
(require 'auto-complete-love)

(add-hook 'lua-mode-hook '(lambda ()
                            (global-company-mode)
                            (setq company-idle-delay 0)
                            (setq ac-sources '(ac-source-love))
                            (push ac-source-lua ac-sources)
                            (auto-complete-mode)))


;; --- Processing ---
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

(autoload 'processing-snippets-initialize "processing-snippets" nil nil nil)
(eval-after-load 'yasnippet '(processing-snippets-initialize))

(setq processing-location "/opt/Processing/processing-java")
(setq processing-application-dir "/opt/Processing")
(setq processing-sketchbook-dir "/home/etomort/tomas/processing/sketchbook")
(setq processing-output-dir "/tmp")

(defun processing-mode-init ()
  "Function used for Processing mode hook."
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-dictionary ac-source-yasnippet))
  (make-local-variable 'ac-user-dictionary)
  (setq ac-user-dictionary (append processing-functions
                                   processing-builtins
                                   processing-constants))
  (local-set-key (kbd "C-<f11>") 'processing-sketch-run)
  (local-set-key (kbd "C-<f1>") 'processing-find-in-reference)
  )

(add-to-list 'ac-modes 'processing-mode)
(add-hook 'processing-mode-hook 'processing-mode-init)


;; --- Python... this is incomplete
(elpy-enable)
(defun my-python-hook ()
  "This is my python hook function."
  (elpy-use-ipython)
  )
(add-hook 'python-mode-hook 'my-python-hook)

(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  "Disable AC (auto-complete mode) for python mode, as elpy use company."
  (unless (eq major-mode 'python-mode) ad-do-it))
(ad-activate 'auto-complete-mode)


;; --- Astyle, uncrustify, GNU indent, etc.. TODO
;; http://stackoverflow.com/questions/1046547/is-there-an-automatic-source-code-formatter-that-nicely-wraps-lines-of-c-c
(defun astyle-this-buffer (pmin pmax)
  "Still PMIN PMAX ... TODO."
  (interactive "r")
  (shell-command-on-region pmin pmax "astyle --style=java -y -xC100" ;; add options here...
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*") t))


;; --- Sublimity ---
(require 'sublimity)
(sublimity-mode 1)
(sublimity-map)
;; ;; (require 'sublimity-scroll)
;; You may configure the speed of smooth-scroll by setting two variables.
;; (setq sublimity-scroll-weight 10
;;       sublimity-scroll-drift-length 5)

;; (require 'sublimity-map)
;; (setq sublimity-map-size 20)
;; (setq sublimity-map-fraction 0.3)
;; (setq sublimity-map-text-scale -7)
;; (sublimity-map-set-delay 5)


;; --- helm spotify has errors if called without debug-on-error set.
(defun tom-spotify ()
  "Wrapper for calling spotify from keyboard shortcut and removing possibility for error."
  (interactive)
  (setq debug-on-error t)
  (helm-spotify)
  (setq debug-on-error nil))t


;; --- Custom stuff
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hc-tab ((t (:background "wheat")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes (quote ("4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default)))
 '(flycheck-clang-language-standard nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(vc-follow-symlinks t))

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
