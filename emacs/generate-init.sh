#!/bin/bash
# This script will concatenate all the desired .el files into init.el

echo "*** Generating DEFAULT configuration:"

FILES="begin.el
package.el
theme.el
bars-and-title.el
column-and-line-numbers.el
paren-indent.el
calendar.el
cond-mac-linux-win.el
duplicate-line.el
xah-cut-copy.el
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

google-translate.el
magit.el
term.el
org-mode.el
nxml.el
markdown.el

gdscript.el
edit-with-emacs.el
python.el

keybindings.el
compilation.el
cua.el
end.el"

rm -f init.el 
echo $FILES | sed 's/ /\n/g' | while read f; do
    echo "Adding "$f ;
    echo -e "\n;; *** FILE: " $f >> init.el;
    cat $f >> init.el;
done 
echo "*** New init.el generated."
echo 
