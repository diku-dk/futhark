.. role:: ref(emphasis)

.. _futhark-lsp(1):

===========
futhark-lsp
===========

SYNOPSIS
========

futhark lsp

DESCRIPTION
===========

Start serving the language server protocol over stdin and stdout.
This is only the server part of the protocol, you likely want to use it via
your editor.
This will enable your editor to provide e.g. hints or hover information by
querying the futhark compiler.

FEATURES
========

Hover Information

  Provides the type of the symbol under the cursor.
  Does not work on the definition of a top-level symbol itself, only references
  to it.

Go To Definition

  Jumps to the place of definition of the symbol under the cursor.

Formatting

  Invokes ``futhark fmt`` to format the current file.

Evaluation Comments

  Comments like ``-- >>> factorial 5`` will be picked up, the editor may offer
  code lenses to evaluate the code contained in the comment.
  Activating the code lens will load the file into the interpreter,
  evaluate the expression and write the result below.

  The evaluation will be aborted after 15 seconds or when it has allocated
  100 GB in total, only the last 100 debugging traces will be retained.

Inlay Hints

  Provides virtual text hints to visualize the results of type-checking.
  Inferred types of bindings will be shown for e.g. lambda arguments, function
  arguments, let bindings or loop bindings.

SEE ALSO
========

:ref:`futhark-fmt(1)`
