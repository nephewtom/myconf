(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.md.html$" . markdown-mode)
         ("\\.text$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))

  :bind (("C-M-m" . livedown:preview)
         :map markdown-mode-map
         ("C-c C-c t" . markdown-toc/generate-toc)
         ("M-p" . nil)
         ("M-n" . nil))

  :config
;;   (setq markdown-open-command "haroopad")
;;   (setq markdown-command "/home/etomort/myconf/bin/flavor.rb"))
;; ;;  (setq markdown-command nil)

;; From: https://github.com/shime/emacs-livedown
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))

;; From: http://increasinglyfunctional.com/2014/12/18/github-flavored-markdown-previews-emacs/
(use-package livedown
  ;;  :ensure t
  )




;; https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time

(defun markdown-filter-for-impatient-mode (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(defun markdown-impatient-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Tom Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

;; Gives error...
;;(imp-set-user-filter markdown-filter-for-impatient-mode)


