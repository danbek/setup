#!/bin/bash

# Argument is the destination path that org-download passes in
OUTFILE="$1"

echo "foo" > ~/notes/images/foo.txt
# Try BMP first, then convert to PNG
#wl-paste --type image/bmp | convert bmp:- "$OUTFILE"
#wl-paste --type image/bmp | convert bmp:- ~/notes/images/foo.png

