;; TODO: Print date in scratch buffer
;; (message (format-time-string "%H:%M:%S.%3N"))
;; (setq myscratch (get-buffer "*scratch*"))
;; (print (format-time-string "%H:%M:%S.%3N") myscratch)
;; (print "rubbish"  myscratch)

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
(defalias 'rf 'recentf-open-files)
(defalias 'recf 'recentf-open-files)

;; --- Font-size & split-pane size
;; For some reason this get disabled if I put it in keybinding.el
(global-set-key (kbd "C-=") 'text-scale-adjust)

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

