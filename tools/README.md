Futhark tools
=============

This directory contains useful programs for working with and and on the futhark compiler. Below are the (non-comprehensive) descriptions of some of the tools in this directory.

Style
-------------
`style-check`. The futhark CI does a style check using the following style checker. It's convienant to run it locally before making a PR, as merging will be blocked if it doesn't pass. `style-check.sh` assumes that [hlint](https://github.com/ndmitchell/hlint) is installed.
```bash
./style-check.sh src/futhark.hs
```
Similarly  `run-formatter.sh` is a convienent tool for automatic code formatting. Useful for catching some obvious issues that trigger the style checker. It requires [ormolu](https://github.com/tweag/ormolu) to be installed. It can be run on files or whole directories.
```bash
./run-formatter.sh src src-testing
```

```
GtkSourceView
-------------

To install the Futhark syntax highlighter for GtkSourceView (e.g. used by
Gedit), copy `futhark.lang` and place it in the following directory:

    ~/.local/share/gtksourceview-3.0/language-specs/

Restart Gedit and open a `*.fut`-file.

Miscellaneous
-------------
`generate-module-list.sh` generates a list of exposed modules for the .cabal file (excluding the parser files). This should be run from futhark's root directory.
```
./tools/generate-module-list.sh
```
