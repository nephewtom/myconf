(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'rtags)
(cmake-ide-setup)

;; This is repeated in init-packages.el
;; (require 'company)
;; (setq company-global-modes '(not emacs-lisp-mode processing-mode text-mode))
;; (global-set-key (kbd "M-y") 'company-complete)


(require 'google-c-style) ;; TODO: Not tested

(add-hook 'c-mode-common-hook 'my-after-c-hook)
(defun my-after-c-hook ()
  "This is my hook after c-mode-common-hook."
  (global-company-mode)
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-clang)
  (add-to-list 'company-backends 'company-c-headers)
  (setq company-idle-delay 0)

  (subword-mode)
  (google-set-c-style)
  (google-make-newline-indent)
  (setq c-basic-offset 4)
  )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; ;; --- C/C++ ---

;; ;; Switches between .h & .cpp files in C/C++
;; (global-set-key (kbd "C-x C-o") 'ff-find-other-file)
;; ;; (global-set-key (kbd "<f4>") 'ff-find-other-file) ;; like QtCreator

;; ;; Disable AC mode in for c++
;; (defadvice auto-complete-mode (around disable-auto-complete-for-c++)
;;   "Disable AC (auto-complete mode) for c mode, as I use company."
;;   (unless (or (eq major-mode 'c++-mode) (eq major-mode 'c-mode)) ad-do-it))
;; (ad-activate 'auto-complete-mode)


;; ;; Considering _ part of a word
;; ;; (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
