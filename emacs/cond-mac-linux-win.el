(cond
 ;; --- Mac OS X stuff ---
 ((string-equal system-type "darwin")
  (message "System: Mac")
  (setq mac-option-modifier 'control)
  (setq mac-command-modifier 'meta)

  (global-set-key (kbd "M-w") 'kill-this-buffer) ;; this works on Mac too
  (define-key global-map (kbd "C-<f2>")
    (lambda ()
      (interactive)
      (x-popup-menu (list '(0 0) (selected-frame))
                    (mouse-menu-bar-map))))
  (set-face-attribute 'default nil :height 200)
  (global-set-key (kbd "<f1> 7") 'browse-url-at-point)
  (global-set-key (kbd "<home>") 'move-beginning-of-line)
  (global-set-key (kbd "<end>") 'move-end-of-line)
  )

 ;; --- Windows stuff ---
 ((string-equal system-type "windows-nt")
  (message "System: Windows")
  (set-face-attribute 'default nil :family "Consolas" :height 140)
  (add-to-list 'exec-path "%HOME%/scoop/apps/git/current/usr/bin")
  (setenv "PATH" (mapconcat #'identity exec-path path-separator))

  (add-to-list 'exec-path "c:/Users/etomort/hunspell/bin/")
  (setq ispell-program-name (locate-file "hunspell"
                                         exec-path exec-suffixes 'file-executable-p))
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq-default default-buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (setq find-program "%HOME%/scoop/apps/git/current/usr/bin/find.exe")
  (setq compile-command "build.bat")
  )
 
 ;; --- Linux stuff ---
 ((message "System: Linux")
  (set-face-attribute 'default nil :family "Liberation Mono-14" :height 120)
  (set-frame-font "Liberation Mono-14:antialias=1")

  ;; --- Persistent sessions
  ;; https://github.com/thierryvolpiatto/psession
  ;; https://github.com/emacs-helm/helm/issues/2028
  ;;  (psession-mode 1)

  ;; This makes Emacs on Windows unusable...
  ;; So set it only on Linux
  ;; https://github.com/emacs-helm/helm/issues/1976
  (setq x-wait-for-event-timeout nil)
  )
 )
