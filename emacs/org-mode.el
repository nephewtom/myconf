(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)

(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-<tab>") nil)
                           (define-key org-mode-map (kbd "M-h") nil)
                           (define-key org-mode-map (kbd "M-n") 'org-forward-heading-same-level)
                           (define-key org-mode-map (kbd "M-p") 'org-backward-heading-same-level)
                           ;; (define-key org-mode-map (kbd "\C-ca") 'org-agenda)
                           (define-key org-mode-map (kbd "C-c e") 'org-edit-special)
                           ))

(setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "blue" :weight bold))
        ("IN-PROGRESS" . (:foreground "red" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "forest green" :weight bold))

        ("TRY" . (:foreground "purple" :weight bold))
        ("NOTE" . (:foreground "black" :weight bold))
        ("REVIEW" . (:foreground "purple" :weight bold))
        ("PERMANENT" . (:foreground "red" :weight bold))
        ("CANCELLED" . (:foreground "black" :weight bold))

        ("WTF" . (:foreground "orange" :weight bold)) ;; Color not working
        ))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE")
        (sequence "TRY" "NOTE" "REVIEW" "PERMANENT" "CANCELLED" "|" "WTF")))

(setq org-priority-faces '((?A . (:background "#DD0000"  :foreground "black" :box '(:line-width 2 :style released-button)))
                           (?B . (:background "#A366FF" :foreground "black" :box '(:line-width 2 :style released-button)))
                           (?C . (:background "#00CCFF" :foreground "black" :box '(:line-width 2 :style released-button)))))

(eval-after-load "org"
  '(require 'ox-md nil t))
(eval-after-load "org"
  '(require 'ox-gfm nil t))


;; TODO: To export the current org-file to markdown,
;; TODO: and then export that one to HTML and preview it in browser.
;; TODO: Not working... I am getting:  org-export-md-and-html-preview: Wrong number of arguments: (0 . 0), 1
;; TODO: Learn to debug emacs functions...
;;
;; (defun org-export-md-and-html-preview ()
;;   "Exports org-mode file to markdown, html and previews it in browser"
;;   (interactive)
;;   (markdown-export-and-preview (org-md-export-to-markdown)))

;; or...
;;  (markdown-export-and-preview (find-file '(org-md-export-to-markdown))))


;; Idea from:
;; (defun markdown-export-and-preview ()
;;   "Export to XHTML using `markdown-export' and browse the resulting file."
;;   (interactive)
;;   (browse-url-of-file (markdown-export)))
