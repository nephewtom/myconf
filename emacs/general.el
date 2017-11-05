;; --- General
(setq inhibit-startup-message t)
(tool-bar-mode -1) ;; removes tool-bar
(scroll-bar-mode t)
(setq default-directory "~/myconf/emacs")
(setq frame-title-format '("nephewtom" ": "(:eval (if (buffer-file-name)
                                                      (buffer-file-name) "%b"))))

;; --- Miscellaneous
(setq set-mark-command-repeat-pop t) ;; https://emacs.stackexchange.com/a/2818/6957
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
(delete-selection-mode 1) ;; Allows to delete without kill-ring & inserting over selection.
(global-unset-key (kbd "C-x C-z")) ;; Unbind suspend-frame
(setq split-width-threshold nil) ;; Split window vertically by default
;; https://stackoverflow.com/questions/7997590/how-to-change-the-default-split-screen-direction
