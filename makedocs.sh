#! /bin/sh

# We require to set the path to the markdownize script
# for Python 3  (cf. https://github.com/fredokun/markdownize)
MARKDOWNIZE="python3.2 ../markdownize/markdownize.py"

$MARKDOWNIZE -i defvariant.lisp -o defvariant.md -b '#|' -e '|#' -l 'lisp'

