Futhark tools
=============

Emacs mode
----------

See the file `futhark-mode.el` (contains install instructions).  This
Emacs mode provides both syntax highlighting and automatic indentation.


GtkSourceView
-------------

To install the Futhark syntax highlighter for GtkSourceView (e.g. used by
Gedit), copy `futhark.lang` and place it in the following directory:

    ~/.local/share/gtksourceview-3.0/language-specs/

Restart Gedit and open a `*.fut`-file.
