;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t)

;; TO TEST THIS, USE THE FILE: multiple-cursors.playground


;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-:") 'mc/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:

(global-set-key (kbd "M-;") 'mc/mark-next-like-this)
(global-set-key (kbd "M-/") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-?") 'mc/mark-all-like-this)


