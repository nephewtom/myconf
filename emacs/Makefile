# From: https://oremacs.com/2015/02/24/emacs-speed-test/
# e = emacs
e = /home/etomort/bin/emacs

init:
	$(e) -Q -l profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs &

basic:
	$(e) -Q -mm -l profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath basic.el)\"))" \
	-f profile-dotemacs &

myold:
	$(e) -Q -mm -l profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
	(setq load-file-name \"$(abspath old-init.el)\"))" \
	-f profile-dotemacs &
