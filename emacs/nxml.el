;; --- XML Stuff ---
(use-package sgml
;  :ensure t
  :config
  (setq sgml-basic-offset 4)
  )

(use-package nxml-mode
;  :ensure t
  :mode (("\\.xml$" . nxml-mode)
         ("\\.xsd$" . nxml-mode)
         ("\\.sch$" . nxml-mode)
         ("\\.rng$" . nxml-mode)
         ("\\.xslt$" . nxml-mode)
;;         ("\\.svg$" . nxml-mode)
         ("\\.rss$" . nxml-mode)
         ("\\.wsdl$" . nxml-mode))

  :bind (:map nxml-mode-map
              ("C-c h" . hs-toggle-hiding))
  
  :config
  (setq nxml-child-indent 4 nxml-attribute-indent 4)
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

(use-package auto-complete-nxml
  :ensure t
  :config
  ;(setq auto-complete-nxml-popup-help-key "C-") ;; TODO: Change this key-binding
  (setq auto-complete-nxml-toggle-automatic-key "C-c C-t"))

;; From http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region (BEGIN, END).
You need to have 'nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))


;; To inform where are we in the XML
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun xml-find-file-hook ()
  (when (derived-mode-p 'nxml-mode)
    (which-function-mode t)
    (setq which-func-mode t)
    (add-hook 'which-func-functions 'nxml-where t t)))

(add-hook 'find-file-hook 'xml-find-file-hook t)
