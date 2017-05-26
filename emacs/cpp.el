;; C/C++ Config
;; TODO: rtags & projectile
;; https://github.com/Andersbakken/rtags
;; http://batsov.com/projectile/

;; Some here are from:
;; https://www.reddit.com/r/emacs/comments/2lf4un/how_do_you_make_emacs_work_for_development/
(require 'aggressive-indent) ;; Aggresive indentation
(aggressive-indent-global-mode)      ;; Enable aggressive indent mode everywhere

(semantic-mode 1)            ;; CEDET holdover
(global-ede-mode 1)          ;; CEDET holdover
(setq c-default-style "bsd") ;; BSD/Allman brackets
(setq c-basic-offset 4)      ;; 4-space indent
(add-hook 'c-mode-common-hook 'company-mode)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook 'flycheck-color-mode-line-mode)
(add-hook 'c-mode-common-hook 'linum-mode)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'hideshowvis-minor-mode)


;; TODO: Not tested
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; (Conditional) C/C++ Keybinds
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "C-c j") 'find-tag)))
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "C-x C-o") 'ff-find-other-file)))
;; (add-hook 'c-mode-common-hook
;;           (lambda () (local-set-key (kbd "<M-y>") 'company-complete)))


;; ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;; F12 - recompile
(global-set-key (kbd "<f12>") 'recompile)


;; More stuff like this in: https://www.emacswiki.org/emacs/IndentingC
(c-add-style "microsoft"
              '("stroustrup"
                (c-offsets-alist
                 (innamespace . -)
                 (inline-open . 0)
                 (inher-cont . c-lineup-multi-inher)
                 (arglist-cont-nonempty . +)
                 (template-args-cont . +))))
;; (setq c-default-style "microsoft")


(defun c-reformat-buffer()
    (interactive)
    (save-buffer)
    (setq sh-indent-command (concat
                             "indent -st -bad --blank-lines-after-procedures "
                             "-br -i4 -l79 -ncs -npcs -nut -npsl -fca "
                             "-lc79 -fc1 -cli4 -bap -sob -ci4 -nlp "
                             buffer-file-name))
    (mark-whole-buffer)
    (universal-argument)
    (shell-command-on-region
     (point-min)
     (point-max)
     sh-indent-command
     (buffer-name))
    (save-buffer))
;; (define-key c-mode-base-map [f7] 'c-reformat-buffer)
