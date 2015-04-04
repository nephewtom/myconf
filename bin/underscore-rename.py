#!/usr/bin/python

import os

path = os.getcwd()

# Should do it with os.walk(), check:
# http://stackoverflow.com/questions/2212643/python-recursive-folder-read

filenames = os.listdir(path)
for f in filenames:
    print f
    os.rename(os.path.join(path, f), os.path.join(path, f.replace(' ', '-')))
