#!/bin/bash
# This script will concatenate all the desired .el files into init-cpp.el

FILES="init.begin.el
bars-and-title.el
column-and-line-numbers.el
paren-indent.el
calendar.el
cond-mac-linux-win.el
duplicate-line.el
xah-cut-copy.el
compilation.el
defalias.el
dired.el
helm.el
movement.el
buffers-utils.el
elisp.el
flycheck.el
company.el

cpp.el

keybindings.el
cua.el
init.end.el"

rm -f init-cpp.el 
echo $FILES | sed 's/ /\n/g' | while read f; do
    echo "Adding "$f ;
    echo ";; --- FILE: " $f >> init-cpp.el;
    cat $f >> init-cpp.el;
done 
echo -e "\nNew init-cpp.el generated."
