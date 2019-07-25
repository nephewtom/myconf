#!/bin/bash
# This file should be located in a directory found in env variable PATH

emacsclient -n $1
wmctrl -a emacs
