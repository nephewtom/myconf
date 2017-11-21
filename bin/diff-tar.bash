#! /bin/sh

# From:
# https://github.com/dcolascione/emacs/blob/master/admin/diff-tar-files

# Copyright (C) 2001-2011  Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


if [ $# != 2 ]; then
    cat <<EOF
Usage: $0 OLD-TAR NEW-TAR
Print a diff of the files in OLD-TAR and NEW-TAR.  Used for checking
the contents of Emacs tar files.
EOF
    exit 1;
fi

old_tar=$1
new_tar=$2

old_tmp=/tmp/old.$$
new_tmp=/tmp/new.$$
trap "rm -f $old_tmp $new_tmp; exit 1" 1 2 15

tar tf $old_tar | sed -e 's,^[^/]*,,' | sort > $old_tmp
tar tf $new_tar | sed -e 's,^[^/]*,,' | sort > $new_tmp
diff -u $old_tmp $new_tmp
rm -f $new_tmp $old_tmp
