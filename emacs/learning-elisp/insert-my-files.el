;; I added file-name-as-directory in front of read-directory-name because
;; my first attempt at using this function involved pasting a directory name without a forward trailing slash,
;; which generated an error because this function contemplates that the directory name ends with a forward trailing slash

(defun insert-my-files ()
  (interactive)
  (let ((dir (read-directory-name "Directory to insert: ")))
    (mapc #'(lambda (file) 
              (let ((file-full (concat dir file)))
                (insert-file-contents file-full)))
          (cddr (directory-files dir)))))
