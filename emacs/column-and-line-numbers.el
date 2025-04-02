;; --- Columns, line-numbers, etc.
(column-number-mode t)
(global-display-line-numbers-mode)

;; --- Date format and forcing to use it in dired
(setq ls-lisp-format-time-list '("%e-%b-%Y" "%e-%b-%Y"))
(setq ls-lisp-use-localized-time-format t)
(setq ls-lisp-use-internal 'ls-lisp) ; Forces internal Emacs listing
