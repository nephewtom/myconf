;; C-u C-M-x invokes Edebug
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Edebug.html

(defun fac (n)
  (if (< 0 n)
      (* n (fac (1- n)))
    1))


(fac 3) 
