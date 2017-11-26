;; --- Dired ---
;; TODO: Sort dired by time date as default 
;; https://superuser.com/questions/875241/emacs-dired-sorting-by-time-date-as-default

(use-package dired+
  :ensure t
  :bind (:map dired-mode-map
              ("j" . dired-find-file)
              ("f" . dired-find-alternate-file)
              ("M-p" . backward-paragraph)     
              ("e" . ora-ediff-files)                
              ("F" . find-name-dired)                
              ("P" . peep-dired))
  :config
  (setq diredp-hide-details-initially-flag nil)
  (setq dired-listing-switches "-lkt")
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  ;; Auto-refresh dired on file change
  (setq dired-auto-revert-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd ".") (lambda () (interactive) (find-alternate-file ".."))))

(use-package peep-dired
  :ensure t)

;; Esta funcions usa dired-jump para abrir en dired el buffer actual
(defun open-in-dired ()
  "Show current buffer on dired."
  (interactive)
  (if (buffer-file-name)
      (dired-jump (buffer-file-name))
    (message "This buffer is not a file in the filesystem.")))

(setq find-args "")
(defun find-dired-by-date (dir args)
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		     (read-string "Run find (with args): " find-args
				  '(find-args-history . 1))))
  ;; Set to this value in order to get a find sorted by date
  (setq find-ls-option '("-exec ls -lt {} + | cut -d ' ' -f5-" . "-lt"))
  (find-dired dir args)
  (setq find-ls-option '("-ls" . "-dilsb")))

(defun find-dired-by-size (dir args)
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		     (read-string "Run find (with args): " find-args
				  '(find-args-history . 1))))
  ;; Set to this one to get it sorted by size
  (setq find-ls-option '("-exec ls -lSr {} + | cut -d ' ' -f5-" . "-lSr"))
  (find-dired dir args)
  (setq find-ls-option '("-ls" . "-dilsb")))
