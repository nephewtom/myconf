;; TODO: I am writing a function that renames the buffer from IBuffer.
;; TODO: So I can rename a bunch of *terminal* buffers without switching to them
;; https://emacs.stackexchange.com/questions/33028/rename-buffer-in-ibuffer

;; HINT: To get the user input, e.g.: the new buffer name I may need this:
;; http://ergoemacs.org/emacs/elisp_idioms_prompting_input.html

(defun reib () ; Rename buffer in ibuffer
  "Rename current buffer to a similar name not already taken.
This function is useful for creating multiple shell process buffers
or multiple mail buffers, etc.

Note that some commands, in particular those based on `compilation-mode'
\(`compile', `grep', etc.) will reuse the current buffer if it has the
appropriate mode even if it has been renamed.  So as well as renaming
the buffer, you also need to switch buffers before running another
instance of such commands."
  (interactive)
  (save-match-data
    (let ((base-name (buffer-name)))
      (and (string-match "<[0-9]+>\\'" base-name)
	   (not (and buffer-file-name
		     (string= base-name
			      (file-name-nondirectory buffer-file-name))))
	   ;; If the existing buffer name has a <NNN>,
	   ;; which isn't part of the file name (if any),
	   ;; then get rid of that.
	   (setq base-name (substring base-name 0 (match-beginning 0))))
      (rename-buffer (generate-new-buffer-name base-name))
      (force-mode-line-update))))



;; This is not related to this function, but I liked it.
(defhydra hydra-links (:exit t
                       :columns 1)
  "sample links"
  ("a" (progn (message "selected Link Option 1") (m)) "Link Option 1") 
  ("b" (message "selected Link 2") "Link 2")
  ("c" (message "selected Some other ID") "Some other ID"))
