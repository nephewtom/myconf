;; --- Processing ---
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

(autoload 'processing-snippets-initialize "processing-snippets" nil nil nil)
(eval-after-load 'yasnippet '(processing-snippets-initialize))

(setq processing-location "/opt/Processing/processing-java")
(setq processing-application-dir "/opt/Processing")
(setq processing-sketchbook-dir "/home/etomort/tomas/processing/sketchbook")
(setq processing-output-dir "/tmp")

(defun processing-mode-init ()
  "Function used for Processing mode hook."
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-dictionary ac-source-yasnippet))
  (make-local-variable 'ac-user-dictionary)
  (setq ac-user-dictionary (append processing-functions
                                   processing-builtins
                                   processing-constants))
  (local-set-key (kbd "C-<f11>") 'processing-sketch-run)
  (local-set-key (kbd "C-<f1>") 'processing-find-in-reference)
  )

(add-to-list 'ac-modes 'processing-mode)
(add-hook 'processing-mode-hook 'processing-mode-init)
