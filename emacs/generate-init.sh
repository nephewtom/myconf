#!/bin/bash
# This script will concatenate all the desired .el files into init.el

echo "*** Generating DEFAULT configuration:"

FILES="begin.el
package-conf.el
theme.el
bars-and-title.el
column-and-line-numbers.el
paren-indent.el
calendar.el
cond-mac-linux-win.el
duplicate-line.el
xah-cut-copy.el
keybindings.el
sudo.el
defalias.el
dired-conf.el
neotree.el
ediff.el
helm-conf.el
movement.el
buffers-utils.el
elisp.el
flycheck.el
company-conf.el
google-translate.el
magit.el
org-mode.el
nxml.el
markdown.el
cpp.el
python-conf.el

compilation.el
multiple-cursors.el
cua.el
super-save.el
dashboard.el
end.el"

# TODO: Test that thing
# emacs-reveal.el

rm -f init.el 
echo $FILES | sed 's/ /\n/g' | while read f; do
    echo "Adding "$f ;
    echo -e "\n;; *** FILE: " $f >> init.el;
    cat $f >> init.el;
done 
echo "*** New init.el generated."
echo 
