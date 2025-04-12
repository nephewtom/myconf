#!/bin/bash
# This file should be located in a directory found in env variable PATH

if grep -e icrosoft /proc/version &> /dev/null ; then
    # echo "arg:"$*
    echo "you seem to be running WSL"
    if ! test -f $*; then
        MYPATH=$*
    else 
        MYPATH=`wslpath -w $*`
    fi
    emacsclientw.exe -n $MYPATH
elif uname -a | grep -q "Windows_NT"; then
    echo "busybox detected..."
    emacsclient -n $*
else
    echo "you seem to be running native Linux"
    emacsclient -n $*
    wmctrl -a tom@gnu
fi
