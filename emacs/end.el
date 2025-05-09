;; TODO: Print date in scratch buffer
;; (message (format-time-string "%H:%M:%S.%3N"))
;; (setq myscratch (get-buffer "*scratch*"))
;; (print (format-time-string "%H:%M:%S.%3N") myscratch)
;; (print "rubbish"  myscratch)

(put 'scroll-left 'disabled nil)


;;(load "~/myconf/emacs/rg.el")
;;(load "~/myconf/emacs/smart-line.el")
;;(load "~/myconf/emacs/spaceline.el")

(message "Emacs ready with init.el !")
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Problem when hitting Alt+Tab on Windows
;;(global-set-key (kbd "<Scroll_Lock>") '(lambda () (interactive) nil ))
(global-set-key (kbd "<Scroll_Lock>") 'ignore)

;; --- Recent files stuff
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(defalias 'rf 'recentf-open-files)
(defalias 'recf 'recentf-open-files)

;; --- Font-size & split-pane size
;; For some reason this get disabled if I put it in keybinding.el
(global-set-key (kbd "C-=") 'text-scale-adjust)

;; --- Importing Tsoding simpc-mode


;; (add-to-list 'load-path "~/myconf/emacs")
;; (require 'simpc-mode)
;; ;; Automatically enabling simpc-mode on files with extensions like .h, .c, .cpp, .hpp
;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))


(load-file custom-file)
(require 'recentf)
(recentf-mode 1)
(recentf-open-files)


(defun profile-emacs-startup ()
  "Calculate and display the total time spent during Emacs startup based on log timestamps."
  (interactive)
  (with-current-buffer "*Messages*"
    (let ((begin-timestamp nil)
          (end-timestamp nil))
      ;; Find BEGIN timestamp
      (goto-char (point-min))
      (when (search-forward "Emacs BEGIN" nil t)
        (beginning-of-line)
        (when (looking-at "\\[\\(.*?\\)\\]")
          (setq begin-timestamp (match-string 1))))

      ;; Find END timestamp
      (goto-char (point-min))
      (when (search-forward "Emacs END" nil t)
        (beginning-of-line)
        (when (looking-at "\\[\\(.*?\\)\\]")
          (setq end-timestamp (match-string 1))))

      (if (and begin-timestamp end-timestamp)
          ;; (progn

          ;; Claude did this... TERRIBLE!
          ;; Parse timestamps manually - format is YYYY-MM-DDThh:mm:ss.ssssss
          (let* ((begin-time-parts (split-string begin-timestamp "[T:]"))
                 (end-time-parts (split-string end-timestamp "[T:]"))
                 (begin-seconds-parts (split-string (nth 3 begin-time-parts) "\\."))
                 (end-seconds-parts (split-string (nth 3 end-time-parts) "\\."))

                 (begin-hour (string-to-number (nth 1 begin-time-parts)))
                 (begin-min (string-to-number (nth 2 begin-time-parts)))
                 (begin-sec (string-to-number (nth 0 begin-seconds-parts)))
                 (begin-microsec (string-to-number (nth 1 begin-seconds-parts)))

                 (end-hour (string-to-number (nth 1 end-time-parts)))
                 (end-min (string-to-number (nth 2 end-time-parts)))
                 (end-sec (string-to-number (nth 0 end-seconds-parts)))
                 (end-microsec (string-to-number (nth 1 end-seconds-parts)))

                 (begin-total-secs (+ (* begin-hour 3600) (* begin-min 60) begin-sec (/ begin-microsec 1000000.0)))
                 (end-total-secs (+ (* end-hour 3600) (* end-min 60) end-sec (/ end-microsec 1000000.0)))
                 (diff-secs (- end-total-secs begin-total-secs)))

            (format "Emacs startup time: %.3f seconds" diff-secs))
        "Could not find BEGIN and END markers in startup log."))))


(message "Emacs END")
(message (profile-emacs-startup))

