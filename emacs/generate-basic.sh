#!/bin/bash
# This script will concatenate all the desired .el files into basic.el

echo "*** Generating BASIC configuration:"

FILES="begin.el
package.el

bars-and-title.el
column-and-line-numbers.el
paren-indent.el

cond-mac-linux-win.el
xah-cut-copy.el
keybindings.el

defalias.el
dired-conf.el



movement.el
buffers-utils.el
elisp.el






markdown-conf.el 

python-conf.el
compilation.el
multiple-cursors.el
cua.el
super-save-conf.el
anzu-conf.el
basic-special.el"


rm -f basic.el 
echo $FILES | sed 's/ /\n/g' | while read f; do
    echo "Adding "$f ;
    echo -e "\n;; *** FILE: " $f >> basic.el;
    cat $f >> basic.el;
done 
echo "*** New basic.el generated."
echo
