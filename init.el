;;; package --- Emacs init.el file
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
;; Reminder! To untabify a whole buffer, use: C-x h to mark it all
;; and then use: M-x untabify

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode t) ;; automatically revert buffer when file changes

;; changes default Emacs behaviour, allowing to delete without kill-ring & inserting over selection.
(delete-selection-mode 1)
(setq default-directory "~")

;; frame title
(setq frame-title-format '("nephewtom" ": "(:eval (if (buffer-file-name)
                                                      (buffer-file-name) "%b"))))

;; --- Useful functions for buffers, yank, move & its bindings ---

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
(global-set-key (kbd "<f9>") 'indent-buffer) ;; Personal taste

(defun yyank-like-vim (arg)
  "Emulates yy command on vim, copy lines (as many as ARG = prefix argument)."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; From: http://emacswiki.org/emacs/CopyingWholeLines
(global-set-key (kbd "C-y") 'yyank-like-vim)

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

;; Same position as US keyboard layout
(global-set-key (kbd "M-ç") 'delete-horizontal-space)
;; http://stackoverflow.com/questions/445225/emacs-command-to-delete-up-to-non-whitespace-character

(global-set-key (kbd "C-l") 'goto-line) ;; Like Eclipse
(global-set-key (kbd "C-S-r") 'query-replace) ;; Seems to remind me r=replace
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f11>") 'hc-toggle-highlight-tabs)
(global-set-key (kbd "M-ñ") 'eval-expression) ;; Same as US layout
(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-0") 'delete-window)


;; Adjusting Split Pane Size
(global-set-key (kbd "C-x +") 'enlarge-window)
(global-set-key (kbd "C-x -") 'shrink-window)
(global-set-key (kbd "C-x ç") 'shrink-window-if-larger-than-buffer)
(global-set-key (kbd "C-x ñ") 'balance-windows)


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


;; --- Packages ELPA, MELPA, Marmalade ---

;; Needs to be before any package in those. E.g.: It fails to load buffer-move,
;; if (require 'buffer-move) is placed just before this package stuff
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; --- Git & Svn ---

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(delete 'Git vc-handled-backends)

;;(require 'psvn)
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)


;; --- Move text ---

;; It allows you to move the current line using M-up / M-down
;; if a region is marked, it will move the region instead.
(require 'move-text)
(move-text-default-bindings)


;; --- Smart line ---

(setq sml/no-confirm-load-theme t)
(sml/setup)
(add-to-list 'rm-excluded-modes " MRev")
(add-to-list 'rm-excluded-modes " ARev")


;; --- Buffers & Ibuffer stuff ---

;; Remove from Ibuffers the buffers that match these regexp
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*helm")
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
(add-to-list 'ibuffer-never-show-predicates "^\\*Disabled")
(add-to-list 'ibuffer-never-show-predicates "^\\*Help")
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp")

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1))) ;; Update ibuffer automatically


;; --- Emacs windows stuff ---

;; Do I actually use this?
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

;; Reminders:
;; * helm-find: C-x c /
;; * To find from helm-find-files (C-x C-f), press: C-c /
;; * To grep from helm-find-files (C-x C-f), press: C-u M-g s
;; * helm-do-grep: C-x c M-g s
;;
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;; --- Dired ---

(require 'dired )
(setq dired-listing-switches "-lk")
;; move to up directory with '.'
(define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file "..")))
;; Following key is already binded to 'a'
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq dired-auto-revert-buffer t)


;; --- Elisp ---

(require 'hl-defined)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)
(setq ediff-split-window-function 'split-window-horizontally)


;; --- Auto-Complete, hippie-expand & Yasnippet ---

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "C-M-y") 'auto-complete) ;; ????
(global-set-key (kbd "C-M-y") 'auto-complete) ;; By Tom, to test

(global-set-key (kbd "C-ñ") 'hippie-expand)

(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(global-set-key (kbd "C-S-y") 'yas-expand)


;; --- Company, Irony, Clang, C++ stuff  ---

(require 'company)
(setq company-global-modes '(not emacs-lisp-mode processing-mode text-mode))
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


;; --- Irony ---

;; From: https://github.com/Sarcasm/irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; --- C/C++ ---

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

(load "~/.emacs.d/dup-mode.el")


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


;; --- XML Stuff ---
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "wsdl") t) "\\'")
                   'nxml-mode))
;; (add-to-list 'auto-mode-alist '("\\.wsdl\\'" . xml-mode))

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(require 'auto-complete-nxml)
(setq auto-complete-nxml-popup-help-key "C-ñ")
(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")


;; --- Markdown stuff ---

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-open-command "haroopad")
(eval-after-load 'markdown-mode
  '(progn
     (define-key markdown-mode-map (kbd "C-c C-c t") 'markdown-toc/generate-toc)
     (define-key markdown-mode-map (kbd "M-p") nil)
     (define-key markdown-mode-map (kbd "M-n") nil)
     ))

;; From: https://github.com/shime/emacs-livedown
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

(global-set-key (kbd "C-M-m") 'livedown:preview)

;; From: http://increasinglyfunctional.com/2014/12/18/github-flavored-markdown-previews-emacs/
(setq markdown-command "/home/etomort/myconf/bin/flavor.rb")


;; --- Edit with Emacs ---

;; Stuff to edit content in web forms via "Edit with Emacs" Chrome plugin
(require 'edit-server)
(edit-server-start)
(add-to-list 'edit-server-url-major-mode-alist '("^stackoverflow" . markdown-mode))
(add-to-list 'edit-server-url-major-mode-alist '("^emacs\\.stackexchange" . markdown-mode))
(add-to-list 'edit-server-url-major-mode-alist '("^unix\\.stackexchange" . markdown-mode))

(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;; When using firefox plugin itsalltext with Emacs, finish editing on Emacs with C-x #
;; http://psung.blogspot.com.es/2009/05/using-itsalltext-with-emacsemacsclient.html


;; --- Org mode ---
(require 'org)
(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-<tab>") nil)
                           (define-key org-mode-map (kbd "C-y") nil)))

(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "blue" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("IN-PROGRESS" . (:foreground "red" :weight bold))
        ("DONE" . (:foreground "forest green" :weight bold))
        ("CANCELLED" . (:foreground "black" :weight bold))
        ))

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "IN-PROGRESS" "DONE" "CANCELLED")))

(set-face-attribute 'org-priority nil
                    :foreground "aquamarine1"
                    :background "black"
                    :inherit font-lock-keyword-face
                    :inverse-video t
                    :box '(:line-width 2
                           :color "orange"
                           :style released-button))

(global-set-key "\C-ca" 'org-agenda)


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
;;(sublimity-mode 1)
;;(sublimity-map)
;; Descomentar esos 2 si quieres ese plugin.

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


;; --- Extend xah-lookup with spanish & alias ---

(require 'xah-lookup)

;; M-x rae
(defun xah-lookup-drae (&optional φword)
  "Lookup definition of current ΦWORD or text selection in URL `http://buscon.rae.es/drae/srv/search?val='."
  (interactive)
  (xah-lookup-word-on-internet
   φword
   "http://buscon.rae.es/drae/srv/search?val=�"
   xah-lookup-dictionary-browser-function))

;; M-x uee
(defun xah-lookup-linguee (&optional φword)
  "Lookup definition of current ΦWORD or text selection in URL `http://www.linguee.es/espanol-ingles/search?source=auto&query='."
  (interactive)
  (xah-lookup-word-on-internet
   φword
   "http://www.linguee.es/espanol-ingles/search?source=auto&query=�"
   xah-lookup-dictionary-browser-function))

(defalias 'xlgoogle 'xah-lookup-google) ;; M-x xlg
(defalias 'xlwikipedia 'xah-lookup-wikipedia) ;; M-x xlw

(global-set-key (kbd "<f1> 7") 'browse-url-at-point)
(define-key help-map (kbd "8") 'xah-lookup-google)
(define-key help-map (kbd "9") 'xah-lookup-word-definition)
(define-key help-map (kbd "0") 'xah-lookup-linguee)


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
                            (auto-complete-mode)
                            ))


;; --- Emoji stuff... on hold. I think I need unicode-fonts.
;; (require 'emoji-cheat-sheet-plus)
;; (add-to-list 'company-backends 'company-emoji)
;; (add-to-list 'load-path "~/.emacs.d/elpa/emoji-cheat-sheet-plus-20150617.631/emoji-cheat-sheet/")


;; --- Custom stuff
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hc-tab ((t (:background "wheat"))))
 '(magit-item-highlight ((t nil)))
 '(sml/modes ((t (:inherit sml/global :foreground "dark violet" :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(flycheck-clang-language-standard nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(irony-supported-major-modes (quote (c++-mode c-mode objc-mode dup-mode)))
 '(livedown:autostart nil)
 '(livedown:open t)
 '(livedown:port 1337)
 '(org-agenda-files (quote ("~/ONGOING.org")))
 '(org-startup-truncated nil)
 '(safe-local-variable-values
   (quote
    ((org-todo-keywords-faces
      ("IN-PROGRESS" . "red")
      ("TODO" . "blue")
      ("WAITING" . "yellow")
      ("DONE" . "green")
      ("CANCELED" . "black")))))
 '(vc-follow-symlinks t))


;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(find-file "~/ONGOING.org")
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
