# From: https://oremacs.com/2015/02/24/emacs-speed-test/

init:
	emacs -Q -l profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs &

basic:
	emacs -Q -mm -l profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath basic.el)\"))" \
	-f profile-dotemacs &

