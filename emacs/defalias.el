;; defalias for Fast M-x
;; http://ergoemacs.org/emacs/emacs_alias.html

(defalias 'hgrep 'helm-grep-do-git-grep)
(defalias 'hfind 'helm-find)
(defalias 'hman 'helm-man-woman)
(defalias 'hoccur 'helm-occur)
(defalias 'hrec 'helm-recentf)

;; TRY helm-swoop & swiper more...

(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

(defalias 'lb 'list-buffers)
(defalias 'lp 'list-processes)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'difbuf 'ediff-buffers)
(defalias 'diffil 'ediff-files)

(defalias 'trframe 'transpose-frame)

(defalias 'odired 'open-in-dired)

(defalias 'open-in-chrome 'browse-url-of-file)
(defalias 'oichrome 'browse-url-of-file)

(defun start-windows-explorer () (interactive) (shell-command "explorer.exe ."))
(defalias 'wexp 'start-windows-explorer)
