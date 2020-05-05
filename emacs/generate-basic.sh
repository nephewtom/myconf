#!/bin/bash
# This script will concatenate all the desired .el files into basic.el

FILES="init.begin.el
bars-and-title.el
column-and-line-numbers.el
paren-indent.el
cond-mac-linux-win.el
duplicate-line.el
xah-cut-copy.el
dired.el
movement.el
buffers-utils.el
elisp.el
keybindings.el
keybasic.el
cua.el
init.end.el"

rm -f basic.el 
echo $FILES | sed 's/ /\n/g' | while read f; do
    echo "Adding "$f ;
    echo ";; --- FILE: " $f >> basic.el;
    cat $f >> basic.el;
done 
echo -e "\nNew basic.el generated."