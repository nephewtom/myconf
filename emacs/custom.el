(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default)))
 '(flycheck-clang-language-standard nil)
 '(httpd-port 8081)
 '(jdee-flycheck-enable-p nil)
 '(jdee-maven-disabled-p t)
 '(livedown:autostart nil)
 '(livedown:open t)
 '(livedown:port 1337)
 '(org-agenda-files (quote ("~/smip/SMIP-GOING.org")))
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (psession ccls company-lsp lsp-ui lsp-mode helm benchmark-init powershell rg ahk-mode impatient-mode flymd helm-w32-launcher w32-browser vbasense dumb-jump function-args nhexl-mode uniquify rpm-spec-mode prettier-js zenburn-theme yaml-mode yafolding xah-lookup wgrep web-beautify use-package typing transpose-frame tabbar sx switch-window strace-mode stem sqlplus sql-indent speed-type spaceline-all-the-icons smart-mode-line-powerline-theme scss-mode rainbow-mode rainbow-delimiters racket-mode pyenv-mode py-autopep8 puppet-mode projectile processing-snippets processing-mode peep-dired paredit pandoc-mode org2blog org-pandoc openwith move-text monokai-theme micgoline mediawiki markdown-toc magit love-minor-mode log4j-mode load-theme-buffer-local lispy json-mode jedi hl-defined highlight-chars hideshowvis helm-swoop helm-smex helm-gtags helm-etags-plus helm-descbinds helm-css-scss helm-company groovy-mode groovy-imports google-this google-c-style gnuplot-mode ggtags fold-dwim flymake-json flycheck-irony flycheck-color-mode-line flx fill-column-indicator exec-path-from-shell evil etags-select esup elpy elisp-slime-nav edit-server doremi-frm doremi-cmd direx dired-toggle-sudo dired+ diff-hl counsel company-irony company-c-headers color-theme-monokai color-theme-emacs-revert-theme color-theme-eclipse color-theme-buffer-local cmake-mode cmake-ide buffer-move better-defaults bash-completion base16-theme awk-it auto-complete-nxml auto-complete-clang auto-complete-c-headers aggressive-indent)))
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/tomas/SDL/sdl-imgui/imgui/")))
     (company-clang-arguments "-I/home/etomort/tomas/Corange/include" "-I/home/etomort/tomas/Corange/include/assets")
     (company-clang-arguments "-I/home/etomort/tomas/Corange/include/" "-I/home/etomort/tomas/Corange/include/assets/")
     (org-todo-keywords-faces
      ("IN-PROGRESS" . "red")
      ("TODO" . "blue")
      ("WAITING" . "yellow")
      ("DONE" . "green")
      ("CANCELED" . "black")))))
 '(scheme-program-name "petite" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hc-tab ((t (:background "wheat"))))
 '(magit-item-highlight ((t nil))))
