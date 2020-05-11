;; This does not copy it.
;; Just uses thing-at-point (seen on a Xah lee web)
(defun xx ()
  "print current word."
  (interactive)  
  (message "%s" (thing-at-point 'word)))

;; These functions are from EmacsWiki
;; https://www.emacswiki.org/emacs/CopyWithoutSelection

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(nth 0 kill-ring)
(car kill-ring)

(defun copy-thing (begin-thing end-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-thing 1))
          (end (get-point end-thing arg)))
      (copy-region-as-kill beg end))))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'forward-word 'backward-word arg)
  ;;(paste-to-mark arg))

(global-set-key (kbd "<f1>") 'copy-word)


;; I am not using these ones for the moment.
(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))))

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (paste-to-mark arg))

(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (copy-thing 'backward-paragraph 'forward-paragraph arg)
  (paste-to-mark arg))

(defun beginning-of-string (&optional arg)
  "  "
  (re-search-backward "[ \t]" (line-beginning-position) 3 1)
  (if (looking-at "[\t ]")
      (goto-char (+ (point) 1))))

(defun end-of-string (&optional arg)
  " "
  (re-search-forward "[ \t]" (line-end-position) 3 arg)
  (if (looking-back "[\t ]")
      (goto-char (- (point) 1))))

(defun thing-copy-string-to-mark (&optional arg)
  " Try to copy a string and paste it to the mark
     When used in shell-mode, it will paste string on shell prompt by default "
  (interactive "P")
  (copy-thing 'beginning-of-string 'end-of-string arg)
  (paste-to-mark arg))
