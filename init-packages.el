;;; package --- Emacs init-packages.el file
;;; Commentary:

;;; Code:

;; --- Packages ELPA, MELPA, Marmalade ---

;; Needs to be before any package in those. E.g.: It fails to load buffer-move,
;; if (require 'buffer-move) is placed just before this package stuff
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))

;; TODO: Understand how packages are loaded...

;; --- Scroll bar ---
;; Tom question: Why is this needed here?
;; It seems if I put it into init-basic.el, gets disabled after the lines above,
;; regarding package initialize.
(scroll-bar-mode t)


;; --- Git & Svn ---

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x g") 'magit-status)

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
;; https://github.com/Malabarba/smart-mode-line
(setq sml/no-confirm-load-theme t)
(sml/setup)
(add-to-list 'rm-excluded-modes " MRev")
(add-to-list 'rm-excluded-modes " ARev")


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
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
(add-to-list 'ibuffer-never-show-predicates "^\\*Disabled")
(add-to-list 'ibuffer-never-show-predicates "^\\*Help")
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp")
(add-to-list 'ibuffer-never-show-predicates "^\\*JDEE")

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1))) ;; Update ibuffer automatically


;; --- Emacs windows stuff ---

;; Do I actually use this?
(global-set-key [C-next] 'windmove-right)
(global-set-key [C-prior] 'windmove-left)
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
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


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


;; --- Auto-Complete ---

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "C-M-y") 'auto-complete) ;; ????
(global-set-key (kbd "C-M-y") 'auto-complete) ;; By Tom, to test

;; --- Yasnippet & hippie-expand ---

(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil) ;; ???
(define-key yas-minor-mode-map (kbd "TAB") nil) ;; ???
(global-set-key (kbd "C-S-y") 'yas-expand)

(global-set-key (kbd "C-;") 'hippie-expand)

;; --- Company ---

(require 'company)
(setq company-global-modes '(not emacs-lisp-mode processing-mode text-mode))
(global-set-key (kbd "M-y") 'company-complete)

(load "~/.emacs.d/dup-mode.el")


;; --- Flycheck / other language related keys ---

(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "<f12>") 'recompile)
(global-set-key (kbd "C-,") 'iedit-mode)

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
(setq compilation-exit-message-function 'compilation-exit-autoclose)


;; --- etags, ggtags

;; TODO: Review this
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting."
  (interactive)
  (find-tag (find-tag-default)))
;; don't prompt when finding a tag
(global-set-key (kbd "M-.") 'find-tag-no-prompt)

;; M-, is binded to tags-loop-continue
;; M-* is binded to pop-tag-mark
;; But I prefer to swap them
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-*") 'tags-loop-continue)


;; --- XML Stuff ---
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "wsdl") t) "\\'")
                   'nxml-mode))
;; (add-to-list 'auto-mode-alist '("\\.wsdl\\'" . xml-mode))

(require 'sgml-mode)
(setq sgml-basic-offset 4)
(require 'nxml-mode)
(setq nxml-child-indent 4 nxml-attribute-indent 4)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))


(require 'auto-complete-nxml)
(setq auto-complete-nxml-popup-help-key "C-ñ") ;; TODO: Change this key-binding
(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")

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


;; --- Hideshow ---
(require 'hideshow)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(add-hook 'nxml-mode-hook 'hs-minor-mode)
(add-hook 'html-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)
(define-key html-mode-map (kbd "C-c h") 'hs-toggle-hiding)

;; (eval-after-load "hs-minor-mode"
(define-key hs-minor-mode-map (kbd "C-c @ C-h") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-s") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-M-h") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-M-s") nil)
(define-key hs-minor-mode-map (kbd "C-c @ C-c") nil)
(define-key hs-minor-mode-map (kbd "C-c <left>") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c h") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c <right>") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c s") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c <up>") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c M-h") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c <down>") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-c M-s") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-c C-c") 'hs-toggle-hiding)

;; TODO: Check yafolding mode. It looks very simple and easy to use.
;; TODO: It has very easy key-bindings, though C-RET collide with cua-set-rectangle-mark
;; TODO: Check: https://github.com/zenozeng/yafolding.el
;; *** Bindings from yafolding.el ***
;; (defvar yafolding-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
;;     (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
;;     (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
;;     map))


;; --- Markdown stuff ---

;;(require 'markdown-mode) ;; Commented because it is breaking color-theme-print
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
                           (define-key org-mode-map (kbd "C-y") nil)
                           (define-key org-mode-map (kbd "M-n") 'org-forward-heading-same-level)
                           (define-key org-mode-map (kbd "M-p") 'org-backward-heading-same-level)
                           (define-key org-mode-map (kbd "\C-ca") 'org-agenda)
                           ))

(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "blue" :weight bold))
        ("IN-PROGRESS" . (:foreground "red" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "forest green" :weight bold))

        ("TRY" . (:foreground "purple" :weight bold))
        ("NOTE" . (:foreground "black" :weight bold))
        ("REVIEW" . (:foreground "purple" :weight bold))
        ("PERMANENT" . (:foreground "red" :weight bold))
        ("CANCELLED" . (:foreground "black" :weight bold))

        ("WTF" . (:foreground "orange" :weight bold)) ;; Color not working
        ))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE")
        (sequence "TRY" "NOTE" "REVIEW" "PERMANENT" "CANCELLED" "|" "WTF")))

(setq org-priority-faces '((?A . (:background "#DD0000"  :foreground "black" :box '(:line-width 2 :style released-button)))
                           (?B . (:background "#A366FF" :foreground "black" :box '(:line-width 2 :style released-button)))
                           (?C . (:background "#00CCFF" :foreground "black" :box '(:line-width 2 :style released-button)))))

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
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  "Disable AC (auto-complete mode) for python mode, as elpy use company."
  (unless (eq major-mode 'python-mode) ad-do-it))
(ad-activate 'auto-complete-mode)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


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
(defun xah-lookup-drae (&optional *word)
  "Lookup definition of current *WORD or text selection in URL 'http://dle.rae.es/?w='."
  "Note: this is the old address `http://buscon.rae.es/drae/srv/search?val='."
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-drae 'xah-lookup-url )
   (get 'xah-lookup-drae 'xah-lookup-browser-function  )))

(put 'xah-lookup-drae 'xah-lookup-url "http://dle.rae.es/?w=word02051")
(put 'xah-lookup-drae 'xah-lookup-browser-function xah-lookup-browser-function)

;; M-x uee
(defun xah-lookup-linguee (&optional *word)
  "Lookup definition of current *WORD or text selection in URL `http://www.linguee.es/espanol-ingles/search?source=auto&query='."
  (interactive)
  (xah-lookup-word-on-internet
   *word
   (get 'xah-lookup-linguee 'xah-lookup-url )
   (get 'xah-lookup-linguee 'xah-lookup-browser-function )))
 
(put 'xah-lookup-linguee 'xah-lookup-url  "http://www.linguee.es/espanol-ingles/search?source=auto&query=word02051")
(put 'xah-lookup-linguee 'xah-lookup-browser-function xah-lookup-browser-function)

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


;; --- Chez Scheme ---
(autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t)
(autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t)
(add-hook 'scheme-mode-hook 'balanced-on)
(custom-set-variables '(scheme-program-name "petite"))
(setq scheme-program-name "petite")


;; --- Emoji stuff... on hold. I think I need unicode-fonts.
;; (require 'emoji-cheat-sheet-plus)
;; (add-to-list 'company-backends 'company-emoji)
;; (add-to-list 'load-path "~/.emacs.d/elpa/emoji-cheat-sheet-plus-20150617.631/emoji-cheat-sheet/")

;; --- term stuff ---

(defadvice term-line-mode (after term-line-mode-fixes ())
  "Enable cua and transient mark modes in term-line-mode."
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'transient-mark-mode) t))
(ad-activate 'term-line-mode)

(defadvice term-char-mode (after term-char-mode-fixes ())
  "Disable cua and transient mark modes in term-char-mode."
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)


;; --- Needs to be here cause it was overwriten by other package
(global-set-key (kbd "M-z") 'recenter-top-bottom) ;; Almost as zz zt...


;; --- Custom stuff
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hc-tab ((t (:background "wheat"))))
 '(magit-item-highlight ((t nil)))
 '(sml/modes ((t (:inherit sml/global :foreground "dark violet" :weight bold)))))

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; init-packages.el ends here
