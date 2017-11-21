#!/bin/bash
find . -type f -exec file {} \; | awk -F: '{if ($2 ~/image/) print $1}'
