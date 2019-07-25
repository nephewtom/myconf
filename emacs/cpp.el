;; C/C++ Config

;; *** Keybinding ***

;; [M-o]   ff-find-other-file : switch-toggle from .h to .cpp, or jumps to file in #include ones.
;; [M-y]   company-complete : complete at point.
;; [C-;]   company-files : complete filesystem files at point.
;; [C-o] helm-imenu : list functions, member functions, attributes, etc... on current file.
;; [C-M-h] helm-cscope-find-calling-this-function : 
;;         fa-show : shows function signature

;; --- Solution to "jump to definition", etc.
;; https://github.com/jacktasia/dumb-jump
;; Put a .dumbjump with + or - (to include/exclude directories in the search)
(use-package dumb-jump
  :bind (:map dumb-jump-mode-map
              ("M-." . dumb-jump-go)
              ("M-," . dumb-jump-back)
              ;; ("M-g i" . dumb-jump-go-prompt)
              ;; ("M-g x" . dumb-jump-go-prefer-external)
              ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)
              )
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'ag)
  ;;(setq dumb-jump-selector 'helm)
  )

;; Traditional TAGS
;; [M-.]   xref-find-definitions : find definition at point.
;; [M-,]   xref-pop-marker-stack : pop back after finding definition.

;; --- Testing Flycheck include path
;; https://stackoverflow.com/questions/24097839/how-to-add-include-path-to-flycheck-c-c-clang
;; (add-hook 'c++-mode-hook
;;           (lambda () (setq flycheck-clang-include-path
;;                            (list (expand-file-name "~/tomas/SDL/sdl-imgui/imgui/")))))




;; --- Useful Alias
(defun make ()
  (interactive)
  (compile "make"))

(defun mclean ()
  (interactive)
  (compile "make clean; make"))

(defun ctags ()
  (interactive)
  (compile "ctags -R -e --c-kinds=cdefglmnpstuvx ."))

(defun pacman ()
  (interactive)
  (compile "cd ~/tomas/SDL/pacman/game/; ./pacman"))

(defun run ()
  (interactive)
  (setq program (concat "./" (file-name-sans-extension (buffer-name))))
  (setq binprogram (concat "./bin/" (file-name-sans-extension (buffer-name))))
  (if (file-exists-p program)
      (compile program)
    (if (file-exists-p binprogram)
        (compile binprogram)
      (message "%s does not exist" program))))

(defun mr ()
  (interactive)
  (setq program (concat "./" (file-name-sans-extension (buffer-name))))
  (setq binprogram (concat "./bin/" (file-name-sans-extension (buffer-name))))
  (if (file-exists-p program)
      (compile (concat "make;" program))
    (if (file-exists-p binprogram)
        (compile (concat "make;" binprogram)))))



;;(define-key c++-mode-map (kbd "C-M-h") nil) ;; unbind from select function
(use-package cc-mode
  :config
  (unbind-key "C-M-h" c++-mode-map)
  (dumb-jump-mode)
  )


;; https://www.reddit.com/r/emacs/comments/2lf4un/how_do_you_make_emacs_work_for_development/
(require 'aggressive-indent) ;; Aggresive indentation
(aggressive-indent-global-mode)      ;; Enable aggressive indent mode everywhere
(which-function-mode)

(semantic-mode 1)            ;; CEDET holdover
(global-ede-mode 1)          ;; CEDET holdover
(setq c-default-style "bsd") ;; BSD/Allman brackets
(setq c-basic-offset 4)      ;; 4-space indent
;;(add-hook 'c-mode-common-hook 'flycheck-color-mode-line-mode)

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'hideshowvis-minor-mode)

;; (Conditional) C/C++ Keybinds
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "M-o") 'ff-find-other-file)))


;; --- Function args
;; fa-show will show the function signature
(require 'function-args)

(fa-config-default)
(set-default 'semantic-case-fold t)
(define-key function-args-mode-map (kbd "M-o") nil) ;; unbind from default
(define-key function-args-mode-map (kbd "M-n") nil) ;; unbind from default
(define-key function-args-mode-map (kbd "M-u") nil) ;; unbind from default
(define-key function-args-mode-map (kbd "M-j") nil) ;; unbind from default
(define-key function-args-mode-map (kbd "M-h") nil) ;; unbind from default




;; --- Indent tool

;; indent --standard-output --blank-lines-after-declarations --blank-lines-after-procedures --braces-on-if-line --indent-level2 --line-length120 --no-space-after-casts --no-space-after-function-call-names --no-tabs --dont-break-procedure-type --format-all-comments --comment-line-length120 --case-indentation2 --blank-lines-after-procedures --swallow-optional-blank-lines --dont-line-up-parentheses

(defun c-reformat-buffer()
  (interactive)
  (save-buffer)
  (setq sh-indent-command (concat
                           "indent -st -bad --blank-lines-after-procedures "
                           "-br -i2 -l120 -ncs -npcs -nut -npsl -fca "
                           "-lc120 -fc1 -cli2 -bap -sob -ci4 -nlp "
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



;; --- Astlye tool
(defun astyle-this-buffer (pmin pmax)
  (interactive "r")
  (shell-command-on-region pmin pmax
                           "astyle" ;; add options here...
                           (current-buffer) t 
                           (get-buffer-create "*Astyle Errors*") t))



;; --- Styles
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



;; --- TO CHECK: rtags & projectile
;; https://github.com/Andersbakken/rtags
;; http://batsov.com/projectile/
;; http://oremacs.com/2017/03/28/emacs-cpp-ide/
