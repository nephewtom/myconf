;; --- Elisp related
(require 'hl-defined)

(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)

;; http://emacsredux.com/blog/2014/06/18/quickly-find-emacs-lisp-sources/
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

