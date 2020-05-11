;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; This is just a text written in the Emacs scratch buffer.
;; Nothing really great, just a bunch of words.
;; I am doing it in order to test the way region, active region and transient-mark-mode work.
;; Let's try it...

;; But first lets add some more lines here.
;; And start doing the magic...
(region-beginning)

(mark-active)
(set-mark)
(defun my-check-region ()
  "print whether region is active."
  (interactive)
  (if (use-region-p)
      (message "region active")
    (message "region not active")))
(my-check-region)


(require 'org-location-google-maps)
(require 'google-maps-static-show)

(google-maps-static-show
 :center "Cimetière du Montparnasse"
 :maptype 'hybrid
 :zoom 5
 :markers '((("Place Saint-Michel, Paris") . (:label ?M :color "blue"))
            (("Jardin du Luxembourg, Paris" "Parc Montsouris, Paris")
             . (:label ?P :color "green")))
 :visible '("44 rue de l'Ouest, Paris" "Montrouge")
 :paths '((("Tour Eiffel, Paris" "Arc de triomphe, Paris" "Panthéon, Paris")
           . (:weight 3 :color "black" :fillcolor "yellow"))))

(setq frame-resize-pixelwise t)
(set-frame-position (selected-frame) 0 0)
(set-frame-size (selected-frame) 1024 600 t)


(insert-image (create-image "/home/etomort/Pictures/smartCenter.png"))  

(image-type-available-p 'imagemagick)


(insert-image (create-image "/home/etomort/Pictures/storyteller.png" 'imagemagick nil :width 500)) 

;;
