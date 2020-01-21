(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.md.html$" . markdown-mode)
         ("\\.text$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))

  :bind ( ;;("C-M-m" . livedown:preview)
         :map markdown-mode-map
         ("C-c C-c t" . markdown-toc/generate-toc)
         ("M-p" . nil)
         ("M-n" . nil))

  :config
  (setq markdown-open-command "markdownmonster.exe")
  ;;(setq markdown-command "markdown")
  )





;; https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time

(defun markdown-html-no-title (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Tom Cooking Markdown...</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;; Gives error...
;;(imp-set-user-filter 'markdown-html)

;; (setq imp-user-filter 'markdown-filter-for-impatient-mode)
;; (cl-incf imp-last-state)
;; (imp--notify-clients)

(defun markdown-preview-browser ()
  (interactive)
  (impatient-mode 1)
  (setq imp-user-filter #'markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients))
