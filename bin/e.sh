#!/bin/bash
# This file should be located in a directory found in env variable PATH

if  grep -e Microsoft /proc/version &> /dev/null ; then
    # echo "arg:"$*
    if ! test -f $*; then
        MYPATH=$*
    else 
        MYPATH=`wslpath -w $*`
    fi
    emacsclientw.exe -n $MYPATH
else
    emacsclient -n $*
    wmctrl -a tom@gnu
fi
