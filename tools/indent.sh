#!/bin/sh
#
# Indent a Futhark file using our (perpetually experimental) Emacs
# futhark-mode.  Output the Emacs-indented version on standard out.
# This can also be used to automatically test if the indenter performs
# as expected, but that is not currently done.

inpfile="$1"

emacs --quick --batch \
      --eval="(add-to-list 'load-path"' ".")' \
      --eval="(require 'futhark-mode)" \
      --eval='(find-file "'"$inpfile"'")' \
      --eval='(indent-region (point-min) (point-max))' \
      --eval='(princ (buffer-string))' \
      2>/dev/null
