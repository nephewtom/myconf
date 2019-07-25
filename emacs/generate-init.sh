#!/bin/bash
# This script will concatenate all the desired .el files into init.el
cat init.begin.el > init.el
while read f; do echo $f; cat $f >> init.el; done < init-concat-files
cat init.end.el >> init.el
