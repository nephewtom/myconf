;; Provide timestamp to *Messages* logs
(load "~/myconf/emacs/log.el")

;; Try to speed up start-up
(setq-default gc-cons-threshold (* 100 1024 1024))
;;(let ((file-name-handler-alist nil)) "~/myconf/emacs/init.el")

;; Follow git symlinks
(setq vc-follow-symlinks t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(load "~/myconf/emacs/general.el")
(load "~/myconf/emacs/column-and-line-numbers.el")
(load "~/myconf/emacs/paren-indent.el")
(load "~/myconf/emacs/calendar.el")
(load "~/myconf/emacs/cond-mac-linux-win.el")
(load "~/myconf/emacs/duplicate-line.el")
(load "~/myconf/emacs/xah-cut-copy.el")
(load "~/myconf/emacs/compilation.el")
(load "~/myconf/emacs/sudo.el")
(load "~/myconf/emacs/defalias.el")

(load "~/myconf/emacs/dired.el")
(load "~/myconf/emacs/ediff.el")
(load "~/myconf/emacs/helm.el")
(load "~/myconf/emacs/movement.el")
(load "~/myconf/emacs/buffers-utils.el")
(load "~/myconf/emacs/elisp.el")
(load "~/myconf/emacs/flycheck.el")
(load "~/myconf/emacs/company.el")
(load "~/myconf/emacs/xah-lookup.el")
(load "~/myconf/emacs/magit.el")

(load "~/myconf/emacs/term.el")
(load "~/myconf/emacs/org-mode.el")
(load "~/myconf/emacs/nxml.el")
(load "~/myconf/emacs/hideshow.el")
(load "~/myconf/emacs/edit-with-emacs.el")
;;(load "~/myconf/emacs/markdown.el")
(load "~/myconf/emacs/python.el")
;;(load "~/myconf/emacs/cpp.el")

(load "~/myconf/emacs/keybindings.el") ;; has (require 'iso-transl)
(load "~/myconf/emacs/cua.el")


;;(setq custom-file "~/myconf/emacs/custom.el")
;;(load custom-file) 
(put 'scroll-left 'disabled nil)


;;(load "~/myconf/emacs/rg.el")
;;(load "~/myconf/emacs/smart-line.el")
;;(load "~/myconf/emacs/spaceline.el")

(server-start) ;; emacs server
(message "Emacs ready with init.el !")
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Problem when hitting Alt+Tab on Windows
;;(global-set-key (kbd "<Scroll_Lock>") '(lambda () (interactive) nil ))
(global-set-key (kbd "<Scroll_Lock>") 'ignore)

;; --- Recent files stuff
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(defalias 'recf 'recentf-open-files)

;; --- Custom variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lorem-ipsum flymd zenburn-theme yaml-mode yafolding xah-lookup wgrep web-beautify use-package typing transpose-frame tabbar sx switch-window strace-mode stem sqlplus sql-indent speed-type spaceline-all-the-icons smart-mode-line-powerline-theme scss-mode rpm-spec-mode rainbow-mode rainbow-delimiters racket-mode pyenv-mode py-autopep8 puppet-mode psession projectile processing-snippets processing-mode prettier-js peep-dired paredit pandoc-mode org2blog org-pandoc openwith nhexl-mode move-text monokai-theme micgoline mediawiki markdown-toc magit love-minor-mode log4j-mode load-theme-buffer-local lispy json-mode jedi impatient-mode hl-defined highlight-chars hideshowvis helm-swoop helm-smex helm-gtags helm-etags-plus helm-descbinds helm-css-scss helm-company groovy-mode groovy-imports google-this google-c-style gnuplot-mode ggtags function-args fold-dwim flymake-json flycheck-irony flycheck-color-mode-line flx fill-column-indicator exec-path-from-shell evil etags-select esup elpy elisp-slime-nav edit-server dumb-jump doremi-frm doremi-cmd direx dired-toggle-sudo dired+ diminish diff-hl counsel company-irony company-c-headers color-theme-monokai color-theme-emacs-revert-theme color-theme-eclipse color-theme-buffer-local cmake-mode cmake-ide buffer-move better-defaults bash-completion base16-theme awk-it auto-complete-nxml auto-complete-clang auto-complete-c-headers aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(color-theme-monokai)
