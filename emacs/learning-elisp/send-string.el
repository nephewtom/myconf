;; TODO: I am writing a function that sends the current line to other buffer.
;; TODO: The use case is to send them to a windows that has sqlplus running, so it can execute them one by one.
;; TODO: It could also send a bunch of lines (a selected region?)
;; TODO: Is there a tool in Emacs that already does that to sqlplus?

(defun send-line-to-buffer (buffer)
(save-excursion
  (setq beg (beginning-of-line))
  (setq end (end-of-line))
  (comint-send-string (buffer-substring-no-properties beg end)))
       )    
;; (comint-send-string "*sqlplus*" "select count (*) from subscription;")

