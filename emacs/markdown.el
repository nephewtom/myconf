(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.md.html$" . markdown-mode)
         ("\\.text$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))

  :bind ( ;;("C-M-m" . livedown:preview)
         :map markdown-mode-map
         ("C-c C-c t" . markdown-toc-generate-toc)
         ("M-p" . nil)
         ("M-n" . nil)
         ("M-<up>" . markdown-move-up)
         ("M-<down>" . markdown-move-down)
         )

  :config
  (setq markdown-open-command nil)
  (setq markdown-command nil)
  )

;; FINAL NOTE: I decided not to preview Markdown on Emacs since the solutions tend to fail
;; Use vscode or IntelliJ for integrated Preview.

;; https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))


(require 'impatient-mode)
(imp-set-user-filter 'markdown-html)

(defun markdown-preview-browser ()
  (interactive)
  (impatient-mode 1)
  (setq imp-user-filter #'markdown-html)
  (cl-incf imp-last-state)
  (imp--notify-clients))


;; TO REVIEW
;; I modified the code slightly to export my org-mode code as markdown and then display it:
(defun markdown-html-org (buffer)
  (with-current-buffer buffer (org-md-export-as-markdown))
  (princ (with-current-buffer "*Org MD Export*"
           (format "<!DOCTYPE html><html><title>Markdown Preview with impatient-mode</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))
;; .... Works great as long as org-export-show-temporary-export-buffer is nil
