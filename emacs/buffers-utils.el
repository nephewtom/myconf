(defun switch-to-previous-buffer ()
  "Swap to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun indent-buffer ()
  "Select current buffer and indent it."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-auto-revert-mode t) ;; automatically revert buffer when file changes

;; --- Uniquify 
(require 'uniquify)
;;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
;;(toggle-uniquify-buffer-names) ;; Different buffer name for same name files


;; --- Ibuffer ----
(use-package ibuffer
  :config 
  (define-key ibuffer-mode-map (kbd "C-o") nil) ;; unbind from default
  (define-key ibuffer-mode-map (kbd "C-i") nil) ;; unbind from default
  (define-key ibuffer-mode-map (kbd "M-h") 'toggle-ibuffer-groups) ;; unbind from default
  (define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group) ;; unbind from default
  )


;; how-can-i-make-ibuffer-auto-refresh-the-list-of-buffers
;; Not using this one
;; https://emacs.stackexchange.com/a/2178/6957
;;(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1))) ;; Update ibuffer automatically

;; Using this one
;; https://emacs.stackexchange.com/a/2179/6957
(defun my-ibuffer-stale-p (&optional noconfirm)
  ;; let's reuse the variable that's used for 'ibuffer-auto-mode
  (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))

(defun my-ibuffer-auto-revert-setup ()
  (set (make-local-variable 'buffer-stale-function)
       'my-ibuffer-stale-p)
  (set (make-local-variable 'auto-revert-verbose) nil)
  (auto-revert-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-auto-revert-setup)


;; --- Ibuffer extension ---
(use-package ibuf-ext
  :config 
  ;; Following lines eliminates annoying *Minibuf-, *Echo Area, etc., but also *magit: buffers
  ;; (add-to-list 'ibuffer-never-show-predicates " .*")

  ;; Remove the buffers that match these regexp
  (add-to-list 'ibuffer-never-show-predicates " .*NeoTree.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*lsp.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*Metahelp*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*tip*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*git-credential.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*Minibuf-.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*Echo Area.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*Custom.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*Python.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*DOC.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*SPP.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*temp.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*edit.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*ediff-tmp.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*emacs-query.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*autoload.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*spool.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*code-conver.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*helm.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*Deletions.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*http.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*RNC.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*elpy.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*server.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*org.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\*org.*")
  (add-to-list 'ibuffer-never-show-predicates " .*\\Marked.*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm ag")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm mini")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm find")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm grep exts")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm M-x")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm-mode")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm buffers")
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm man woman*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Helm Swoop")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Disabled")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Help")
  (add-to-list 'ibuffer-never-show-predicates "^\\*tramp")
  (add-to-list 'ibuffer-never-show-predicates "^\\*JDEE")
  (add-to-list 'ibuffer-never-show-predicates "^\\*magit.*process")
  (add-to-list 'ibuffer-never-show-predicates "^\\*magit.*diff")
  (add-to-list 'ibuffer-never-show-predicates "^\\*magit.*log")
  )

;; From: http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html


(defun my-ibuffer-saved-groups ()
  (setq ibuffer-saved-filter-groups
        '(("home"
           ("myconf" (or (filename . "myconf")
                         (filename . "emacs-config")))
           ("SMIP" (filename . "smip"))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("playground" (filename . "playground"))
           ("/usr/include/" (filename . "/usr/include/"))
           ("SDL" (filename . "tomas/SDL/"))
           )
          ("default")
          ))
  (ibuffer-switch-to-saved-filter-groups "home" ))


(add-hook 'ibuffer-mode-hook 'my-ibuffer-saved-groups)


(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

;; Ibuffer formats like column width
(define-ibuffer-column vtime
  (:name "Time" :inline t)
  (string ?a ?b ?c ?d)
  )

(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 30 30 :left :elide) ; change: 30s were originally 18s
              " "
              (vtime 4 4 :left)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)

              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))
