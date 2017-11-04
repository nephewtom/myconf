;; These are just experiments I am doing to try to learn more ELisp

;; Work in progress
(defun eval-line ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil)
  (eval-region))
;; 
;; Returns -> Wrong number of arguments: eval-region, 0
(+ 2 2)

(defun mark-n (n)
  "Programmtically mark the next N lines"   
  (interactive "nNum lines to mark: ")
  (push-mark)  
  (next-line n))


("push-mark") () (  jal dj)
