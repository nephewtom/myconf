;; --- Elisp related
(require 'hl-defined)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)

;; http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Disable checkdoc in elisp
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; TESTING
;; (defun find-tag-no-prompt ()
;;   "Jump to the tag at point without prompting"
;;   (interactive)
;;   (find-tag (find-tag-default)))
;; ;; don't prompt when finding a tag

;; (global-set-key (kbd "M-.") 'find-tag-no-prompt)
;; (global-set-key (kbd "M-.") 'find-function-at-point)
;; (global-set-key (kbd "M-.") 'xref-find-definitions)
;; (global-set-key (kbd "M-,") 'pop-tag-mark)

;; (require 'elisp-slime-nav)
;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;   (add-hook hook 'elisp-slime-nav-mode))
