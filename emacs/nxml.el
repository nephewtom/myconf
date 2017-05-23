;; --- XML Stuff ---
(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "wsdl") t) "\\'")
                   'nxml-mode))
;; (add-to-list 'auto-mode-alist '("\\.wsdl\\'" . xml-mode))

(require 'sgml-mode)
(setq sgml-basic-offset 4)
(require 'nxml-mode)
(setq nxml-child-indent 4 nxml-attribute-indent 4)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))


(require 'auto-complete-nxml)
(setq auto-complete-nxml-popup-help-key "C-ñ") ;; TODO: Change this key-binding
(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")

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
