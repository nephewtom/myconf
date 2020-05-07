#!/bin/bash
# This script will concatenate all the desired .el files into init-cpp.el

echo "*** Generating CPP configuration:"

FILES="begin.el
package.el
bars-and-title.el
column-and-line-numbers.el
paren-indent.el
calendar.el
cond-mac-linux-win.el
duplicate-line.el
xah-cut-copy.el

defalias.el
dired.el


helm.el
movement.el
buffers-utils.el
elisp.el
flycheck.el
company.el
xah-lookup.el 









cpp.el
compilation.el
keybindings.el
cua.el
end.el"

rm -f init-cpp.el 
echo $FILES | sed 's/ /\n/g' | while read f; do
    echo "Adding "$f ;
    echo ";; --- FILE: " $f >> init-cpp.el;
    cat $f >> init-cpp.el;
done 
echo "*** New init-cpp.el generated."
echo
