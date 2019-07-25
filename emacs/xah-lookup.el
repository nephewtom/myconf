;; --- Extend xah-lookup with spanish & alias ---
(use-package xah-lookup
  :ensure t
  :config
  ;; M-x rae
  (defun xah-lookup-drae (&optional *word)
    "Lookup definition of current *WORD or text selection in URL 'http://dle.rae.es/?w='."
    "Note: this is the old address `http://buscon.rae.es/drae/srv/search?val='."
    (interactive)
    (xah-lookup-word-on-internet
     *word
     (get 'xah-lookup-drae 'xah-lookup-url )
     (get 'xah-lookup-drae 'xah-lookup-browser-function  )))

  (put 'xah-lookup-drae 'xah-lookup-url "http://dle.rae.es/?w=word02051")
  (put 'xah-lookup-drae 'xah-lookup-browser-function xah-lookup-browser-function)

  ;; M-x uee
  (defun xah-lookup-linguee (&optional *word)
    "Lookup definition of current *WORD or text selection in URL `http://www.linguee.es/espanol-ingles/search?source=auto&query='."
    (interactive)
    (xah-lookup-word-on-internet
     *word
     (get 'xah-lookup-linguee 'xah-lookup-url )
     (get 'xah-lookup-linguee 'xah-lookup-browser-function )))
  
  (put 'xah-lookup-linguee 'xah-lookup-url  "http://www.linguee.es/espanol-ingles/search?source=auto&query=word02051")
  (put 'xah-lookup-linguee 'xah-lookup-browser-function xah-lookup-browser-function)
  
  (defalias 'xlgoogle 'xah-lookup-google) ;; M-x xlg
  (defalias 'xlwikipedia 'xah-lookup-wikipedia) ;; M-x xlw

  :bind (
         :map help-map
         ("7" . browse-url-at-point)
         ("8" . xah-lookup-google)
         ("9" . xah-lookup-word-definition)
         ("0" . xah-lookup-linguee))
  )

(global-set-key (kbd "<f1> 7") 'browse-url-at-point)
(global-set-key (kbd "C-h 6") 'browse-url-of-buffer)

;; TODO: I only want this on WSL
;; https://emacs.stackexchange.com/questions/47782/is-there-a-way-emacs-can-infer-is-running-on-wsl-windows-subsystem-for-linux
(defun browse-url-tom (url &optional new-window)
  (shell-command
   (concat "chrome.exe " url)))
(setq browse-url-browser-function 'browse-url-tom)
