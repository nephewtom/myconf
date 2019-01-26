#!/bin/bash
# https://oremacs.com/2015/02/24/emacs-speed-test/

PROFILE_FILE="~/myconf/emacs/profile-dotemacs.el"
INIT_FILE="~/myconf/emacs/init.el"
if [ -z "$1" ]; then
    echo "No init file provided, taking" $INIT_FILE
else
    INIT_FILE=$1
    echo "Emacs init file"
fi

emacs -Q -l $PROFILE_FILE \
          --eval "(setq profile-dotemacs-file \
                        (setq load-file-name \"$INIT_FILE\"))" \
          -f profile-dotemacs
