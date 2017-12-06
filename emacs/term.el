;; --- terminal stuff ---
(use-package term
  :init
  ;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
  (defun oleh-term-exec-hook ()
    (let* ((buff (current-buffer))
           (proc (get-buffer-process buff)))
      (set-process-sentinel
       proc
       `(lambda (process event)
          (if (string= event "finished\n")
              (kill-buffer ,buff))))))

  (add-hook 'term-exec-hook 'oleh-term-exec-hook)
  ;; https://github.com/jwiegley/use-package/issues/228
  (add-hook 'term-mode-hook (lambda () (setq global-hl-line-mode nil)))
  (setq explicit-shell-file-name "/bin/bash")

  :config
  (setq term-buffer-maximum-size 0) ;; Set unlimited buffer size for terminal

  (defadvice term-line-mode (after term-line-mode-fixes ())
    "Enable cua and transient mark modes in term-line-mode."
    (set (make-local-variable 'cua-mode) t)
    (set (make-local-variable 'transient-mark-mode) t))
  (ad-activate 'term-line-mode)

  (defadvice term-char-mode (after term-char-mode-fixes ())
    "Disable cua and transient mark modes in term-char-mode."
    (set (make-local-variable 'cua-mode) nil)
    (set (make-local-variable 'transient-mark-mode) nil))
  (ad-activate 'term-char-mode)
  )

(defun terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*terminal*")
      (switch-to-buffer "*terminal*")
    (term "/bin/bash"))
  (get-buffer-process "*terminal*"))

(defalias 'tt 'terminal)

(defun dired-open-term ()
  "Open an `terminal' that corresponds to current directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (term-send-string
     (terminal)
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(define-key dired-mode-map (kbd "C-t") 'dired-open-term)
(define-key dired-mode-map (kbd "t") 'dired-open-term)

(defun named-term (name)
  (interactive "sName: ")
  (ansi-term "/bin/bash" name))

