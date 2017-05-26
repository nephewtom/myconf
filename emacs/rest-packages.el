;; --- Test OpenWith --- Could be useful... 
;; https://stackoverflow.com/a/6845470/316232
;; https://www.emacswiki.org/emacs/OpenWith


;; --- Emacs windows stuff ---
;; Do I actually use this?
(global-set-key [C-S-iso-lefttab] 'windmove-up)

(require 'buffer-move)
(defun win-swap ()
  "Swap windows using buffer-move."
  (interactive)
  (if (null (windmove-find-other-window 'right))
      (buf-move-left)
    (buf-move-right)))

;; useful library. Provides: transpose-frame,
;; flip-frame(Vertical), flop-frame(Horizontal),
;; rotate-frame (180º), rotate-frame-clockwise, rotate-frame-anticlockwise
(require 'transpose-frame)



;; --- Auto-Complete ---
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "C-M-y") 'auto-complete) ;; ????
(global-set-key (kbd "C-M-y") 'auto-complete) ;; By Tom, to tes


;; --- Yasnippet & hippie-expand ---

(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil) ;; ???
(define-key yas-minor-mode-map (kbd "TAB") nil) ;; ???
(global-set-key (kbd "C-S-y") 'yas-expand)


;; --- Flycheck / other language related keys ---
;;(add-hook 'after-init-hook #'global-flycheck-mode)



;; --- etags, ggtags
;; TODO: Review this
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting."
  (interactive)
  (find-tag (find-tag-default)))
;; don't prompt when finding a tag
(global-set-key (kbd "M-.") 'find-tag-no-prompt)

;; M-, is binded to tags-loop-continue
;; M-* is binded to pop-tag-mark
;; But I prefer to swap them
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-*") 'tags-loop-continue)


;; --- Other modes
(load "~/myconf/emacs/edit-with-emacs.el")
;; (load "~/myconf/emacs/processing.el")
;; (load "~/myconf/emacs/lua-love.el")
(load "~/myconf/emacs/js-css-html.el")


;; --- Astyle, uncrustify, GNU indent, etc.. TODO
;; http://stackoverflow.com/questions/1046547/is-there-an-automatic-source-code-formatter-that-nicely-wraps-lines-of-c-c
(defun astyle-this-buffer (pmin pmax)
  "Still PMIN PMAX ... TODO."
  (interactive "r")
  (shell-command-on-region pmin pmax "astyle --style=java -y -xC100" ;; add options here...
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*") t))


;; --- Needs to be here cause it was overwriten by other package
(global-set-key (kbd "M-z") 'recenter-top-bottom) ;; Almost as zz zt...


;;; init.el ends here
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; rest-packages.el ends here
