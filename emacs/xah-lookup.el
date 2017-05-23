;; --- Extend xah-lookup with spanish & alias ---
(require 'xah-lookup)

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

(global-set-key (kbd "<f1> 7") 'browse-url-at-point)
(define-key help-map (kbd "8") 'xah-lookup-google)
(define-key help-map (kbd "9") 'xah-lookup-word-definition)
(define-key help-map (kbd "0") 'xah-lookup-linguee)
