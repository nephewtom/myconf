#!/bin/bash
# This file should be located in a directory found in env variable PATH

if  grep -e Microsoft /proc/version &> /dev/null ; then
    emacsclientw.exe -n $*
    wmctrl -a tom@gnu
else
    emacsclient -n $*
    wmctrl -a tom@gnu
fi
