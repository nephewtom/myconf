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
