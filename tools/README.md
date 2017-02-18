Futhark tools
=============

Emacs mode
----------

[![MELPA](https://melpa.org/packages/futhark-mode-badge.svg)](https://melpa.org/#/futhark-mode)

See the file `futhark-mode.el`.  This Emacs mode provides both syntax
highlighting and automatic indentation.

*Installation:* You can install this mode with Emacs' package manager.
Enable the https://melpa.org/ archive, and install the `futhark-mode`
package.  Alternatively, follow the manual install instructions in
`futhark-mode.el`.

Run `futhark-mode.el-corpus-test.sh` to test if the mode's
auto-indentation works.  If you find pieces of code that futhark-mode
cannot indent properly, please fix it, and put small examples into the
`futhark-mode.el-corpus` directory to ensure that it doesn't break in
the future.

### Authors

Initiated by Troels Henriksen in 2013.  Subsequently improved by:

  + Niels G. W. Serup
  + Troels Henriksen
  + Rasmus Wriedt Larsen


GtkSourceView
-------------

To install the Futhark syntax highlighter for GtkSourceView (e.g. used by
Gedit), copy `futhark.lang` and place it in the following directory:

    ~/.local/share/gtksourceview-3.0/language-specs/

Restart Gedit and open a `*.fut`-file.
