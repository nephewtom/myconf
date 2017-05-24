;; WORK IN PROGRESS...

;; (1) First, understand what this function is doing
(string-reverse "samot")

;; (1a) Looking at its function definition
(defsubst string-reverse (str)
  "Reverse the string STR."
  (apply 'string (nreverse (string-to-list str))))

(defsubst string-to-list (string)
  "Return a list of characters in STRING."
  (append string nil))

;; (1b) and at every function that calls.
;;      being most of them built-in functions:

;; (append &rest SEQUENCES)
;; Concatenate all the arguments and make the result a list.
;; The result is a list whose elements are the elements of all the arguments.
;; Each argument may be a list, vector or string.
;; The last argument is not copied, just used as the tail of the new list.

;; (nreverse LIST)
;; Reverse LIST by modifying cdr pointers.
;; Return the reversed list.  Expects a properly nil-terminated list.

;; (string &rest CHARACTERS)
;; Concatenate all the argument characters and make the result a string.

;; (apply FUNCTION &rest ARGUMENTS)
;; Call FUNCTION with our remaining args, using our last arg as list of args.
;; Then return the value FUNCTION returns.
;; Thus, (apply '+ 1 2 '(3 4)) returns 10.

;; (1c) And I played around those functions...
(apply 'string (nreverse (string-to-list str)))
(apply 'string (nreverse (append "samot" nil)))




;; (2) Second, try to look for how to call it interactively.

;; (2a) I arrived to this post:

;; http://stackoverflow.com/questions/14201740/replace-region-with-result-of-calling-a-function-on-region
;; (defun apply-function-to-region (fn)   
;;   (interactive "XFunction to apply to region: ")   
;;   (save-excursion
;;     (let* ((beg (region-beginning))
;;            (end (region-end))
;;            (resulting-text 
;;             (funcall 
;;              fn 
;;              (buffer-substring-no-properties beg end))))
;;       (kill-region beg end)
;;       (insert resulting-text))))

;; So I checked this:
(buffer-substring-no-properties 195 210)

;; And came with this function using setq instead of let:
(defun i-string-reverse ()
  (interactive)
  (setq beg (region-beginning))
  (setq end (region-end))
  (setq region-text (buffer-substring-no-properties beg end))
  (setq reverse-region-text (apply 'string (nreverse (append region-text nil))))
  (kill-region beg end)
  (insert reverse-region-text)
  (message reverse-region-text))
;; But it is not working fine with the region...


;; (2b) I also found this way of doing it in the second answer to that post
(defun apply-to-region (func)
  (unless (use-region-p)
    (error "need an active region"))
  (let ((res (funcall func (buffer-substring (mark) (point)))))
    (delete-region (region-beginning) (region-end))
    (insert res)))

(defun my/test ()
  (interactive)
  (apply-to-region 'string-reverse))




;; (3) Then I found a post with a very good explanation of (interactive "r") and mark stuff
;; https://emacs.stackexchange.com/questions/12334/elisp-for-applying-command-to-only-the-selected-region

(defun i-string-reverse2 (beg end)
  (interactive "r")
  (if (use-region-p)
      (message "The region is active, and is from %d to %d" beg end)
    (message "The region is still there (from % d to %d), but it is inactive" 
             beg end))
  (setq region-text (buffer-substring-no-properties beg end))
  (setq reverse-region-text (string-reverse region-text)) ;; Using string-reverse directly
  (kill-region beg end)
  (insert reverse-region-text)
  (message reverse-region-text))





;; (4) Generalizing it like in (2b) and taking the current line if no region selected
(defun apply-to-region (func &optional beg end)
  (interactive "r")
  (or beg (setq beg (move-beginning-of-line)))
  (or end (setq end (move-end-of-line)))
  (setq res (funcall func (buffer-substring beg end)))
  (delete-region beg end)
  (insert res))

(defun mytest ()
  (interactive)
  (apply-to-region 'string-reverse (region-beginning) (region-end)))

;; Tips for optional parameter from here:
;; https://emacs.stackexchange.com/questions/14199/optional-parameter-defaults



;; test-bed
samot
tomas orti
itro samot
itro samot
itro samot
