#!/bin/bash
# This script will concatenate all the desired .el files into init.el

FILES="init.begin.el
bars-and-title.el
column-and-line-numbers.el
paren-indent.el
calendar.el
cond-mac-linux-win.el
duplicate-line.el
xah-cut-copy.el
compilation.el
sudo.el
defalias.el
dired.el
neotree.el
ediff.el
helm.el
movement.el
buffers-utils.el
elisp.el
flycheck.el
company.el
xah-lookup.el
google-translate.el
magit.el
term.el
org-mode.el
nxml.el
markdown.el
hideshow.el
edit-with-emacs.el
python.el
keybindings.el
cua.el
init.end.el"

rm -f init.el 
echo $FILES | sed 's/ /\n/g' | while read f; do
    echo "Adding "$f ;
    echo ";; --- FILE: " $f >> init.el;
    cat $f >> init.el;
done 
echo -e "\nNew init.el generated."
