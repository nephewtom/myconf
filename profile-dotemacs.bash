#!/bin/bash
# https://oremacs.com/2015/02/24/emacs-speed-test/

INIT_FILE="~/myconf/emacs/init.el"
if [ -z "$1" ]; then
    echo "No init file provided, taking" $INIT_FILE
else
    INIT_FILE=$1
    echo "Emacs init file"
fi

emacs -Q -l ~/myconf/emacs/profile-dotemacs.el \
      --eval "(setq profile-dotemacs-file \
(setq load-file-name \"$INIT_FILE\"))" \
      -f profile-dotemacs
