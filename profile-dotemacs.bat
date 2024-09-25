
# https://oremacs.com/2015/02/24/emacs-speed-test/

emacs -Q -l .\emacs\profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \".\emacs\init.el\"))" -f profile-dotemacs
