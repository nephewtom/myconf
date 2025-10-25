(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "H-i") 'switch-to-buffer)

(eval-after-load "dired" '(progn
                            (define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
                            (put 'dired-find-alternate-file 'disabled nil)
                            (define-key dired-mode-map (kbd ".")
                                        (lambda () (interactive) (find-alternate-file "..")))
                            (define-key dired-mode-map (kbd "C-o") 'find-file)
                            (define-key dired-mode-map (kbd "C-w") 'kill-this-buffer)
                            (setq dired-listing-switches "-lkt")
                            ))
(eval-after-load "ibuffer" '(progn
                              (define-key ibuffer-mode-map (kbd "C-o") nil)
                              (define-key ibuffer-mode-map (kbd "C-i") nil)
                              (define-key ibuffer-mode-map (kbd "M-h") 'toggle-ibuffer-groups)
                              (define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group)
                              ))


;; --- Simpler and faster than helm
(require 'smex)
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                                        ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; --- Importing Tsoding simpc-mode for C/C++
(add-to-list 'load-path "~/myconf/emacs")
(require 'simpc-mode)
;; Automatically enabling simpc-mode on files with extensions like .h, .c, .cpp, .hpp
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))


;; --- company mode stuff
(global-company-mode)
(setq company-idle-delay 0)
(setq company-global-modes '(not processing-mode text-mode)) ;; Not use company on those modes
(add-to-list 'company-backends 'company-dabbrev)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(global-set-key (kbd "M-y") 'company-complete)

(define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)


;; --- Theme and tweaks
(load-theme 'tango-dark)
(global-hl-line-mode t) ;; highlight current line
(make-variable-buffer-local 'global-hl-line-mode)
(set-face-background hl-line-face "#3e4446")
(set-face-foreground hl-line-face "#aeaeac")
(set-face-attribute 'region nil :background "#848000") ;;
(set-face-attribute 'fringe nil :background "#505050")

(face-attribute 'default :background nil)
(face-attribute 'default :foreground nil)


;; --- Super save
(setq super-save-auto-save-when-idle t)
(setq super-save-idle-duration 5)
(super-save-mode 1)

;; Manually add an idle timer
(run-with-idle-timer super-save-idle-duration t #'super-save-command)


;; --- Alias
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
(defalias 'nf 'new-frame)

(defalias 'odired 'open-in-dired)


;; --- Move text
;;; It allows you to move the current line using M-up / M-down
;; If a region is marked, it will move the region instead.
(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

