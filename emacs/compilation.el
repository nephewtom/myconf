(defun my-compilation-hook () 
  "Make sure that the compile window is splitting vertically"
  (progn
    (if (not (get-buffer-window "*compilation*"))
        (progn
          (split-window-vertically)
          ))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)


;; Function for compiling 
(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    ;; Kill the running compilation process if it exists
    (let ((comp-proc (get-buffer-process "*compilation*")))
      (when comp-proc
        (sit-for 0.1)  ;; Give Emacs a short delay to properly kill the process
        (when (process-live-p comp-proc)  ;; If still running, force kill
          (delete-process comp-proc)
          )))

    ;; Now run recompile and force it to not prompt (avoid the "kill it?" prompt)
    (let ((compilation-ask-about-save nil))  ;; Prevent asking about saving buffers
      (call-interactively 'recompile))
    
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 15))
    (select-window cur)
    ))
(global-set-key (kbd "M-<f12>") 'compile)
(global-set-key (kbd "<f12>") 'my-compile)


(defun hide-compilation-buffer ()
  (interactive)
  (let ((w (get-buffer-window "*compilation*")))
    (when w
      (delete-window w))))
(global-set-key (kbd "C-<f12>") 'hide-compilation-buffer)

;; Funtion to run compiled programs
(cond
 ((string-equal system-type "windows-nt")
  (message "System: Windows")
  (setq compile-command "build.bat")

  (defun run-program () (interactive)
         (when (get-buffer "*run*")
           (kill-buffer "*run*"))
         
         (when (get-buffer "*compilation*")
           (delete-window (get-buffer-window (get-buffer "*compilation*")))
           (kill-buffer "*compilation*"))
         (add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)) )
         (async-shell-command "run.bat")
         (switch-to-buffer (get-buffer "*Async Shell Command*"))
         (rename-buffer "*run*")
         (switch-to-previous-buffer)
         )
  
  (defun clean-program () (interactive)
         (when (get-buffer "*clean*")
           (kill-buffer "*clean*"))
         (add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)) )
         (async-shell-command "clean.bat")         
         (switch-to-buffer (get-buffer "*Async Shell Command*"))
         (rename-buffer "*clean*")
         (switch-to-previous-buffer)
         )
  
  ;; (global-set-key (kbd "<f9>") 'run-program)
  ;; (global-set-key (kbd "<f11>") 'clean-program)
  )

 ((message "System: Other"))
 )



;; NOT USED NOW
;; Helper for compilation. Close the compilation window if there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))

;; Specify my function (maybe I should have done a lambda function)
;;(setq compilation-exit-message-function 'compilation-exit-autoclose)

