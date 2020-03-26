#!/bin/bash
DIRTMP=/tmp/.directory-list
DIRLIST=/tmp/directory-list

ls -la | awk '/^d/ { print $0 }' > $DIRTMP
tr -s ' ' < $DIRTMP | cut -d ' ' -f 9- > $DIRLIST
sed -i 1,2d $DIRLIST # Delete . & .. directories
while read f; do du -hs "$f" ; done < $DIRLIST | sort -h
rm -f $DIRTMP $DIRLIST
