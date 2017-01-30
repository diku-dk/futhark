Futhark Basis Library Style Guide
=================================

This short document provides instructions on the coding style used in
the Futhark Basis Library ("futlib").  When you add new code, please
try to adhere to the style outlined here.  To ease porting and
integration, Futhark is designed to permit a variety in naming and
indentation style (so it can adapt to conventions already in use), but
we try to be more consistent in the Basis Library itself.

If you disagree with any of these instructions, feel free to open a
Github issue for discussion.

Style Rules
===========

The style is generally aimed at terseness.  When in doubt, make it
short and simple.

### Line Length

The maximum line length is *80 characters*.

### Indentation

Tabs are illegal. Use spaces for indenting.  Indent your code blocks
with *2 spaces*.

### Capitalisation

All names are lowercase, with `snake_case` used for compound names.

### Comments

Write proper sentences; start with a capital letter and use proper
punctuation.

Give non-trivial top level functions an explanatory comment.  This may
be skipped when the function corresponds to one in a module type.

### Modules

Every module should be matched with some signature.

### Type Annotations

Avoid optional type annotations unless they express some interesting
property (i.e. contain shape declarations).
