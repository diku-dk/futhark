Futhark tools
=============

Emacs mode
----------

See the file `futhark-mode.el` (contains install instructions).  This
Emacs mode provides both syntax highlighting and automatic indentation.

Run `futhark-mode.el-corpus-test.sh` to test if the mode's
auto-indentation works.  If you find pieces of code that futhark-mode
cannot indent properly, please fix it, and put small examples into the
`futhark-mode.el-corpus` directory to ensure that it doesn't break in
the future.


GtkSourceView
-------------

To install the Futhark syntax highlighter for GtkSourceView (e.g. used by
Gedit), copy `futhark.lang` and place it in the following directory:

    ~/.local/share/gtksourceview-3.0/language-specs/

Restart Gedit and open a `*.fut`-file.
