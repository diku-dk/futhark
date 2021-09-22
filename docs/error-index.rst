.. _error-index:

Error index
===========

Elaboration on type errors produced by the compiler.

.. _not-unique:

"*x* has type *y*, which is not unique"
---------------------------------------

This error message occurs for programs that try to perform an in-place
update on arrays that are not "unique".  For example, it would occur
for the following program::

  let f (a: []i32) =
    let a[0] = a[0]+1
    in a

A *unique array type* is written by prefixing the array type with an
asterisk, so the program could be fixed by writing it like this::

  let f (a: *[]i32) =
    let a[0] = a[0]+1
    in a

Note that this places extra obligations on the caller of the ``f``
function, since it now *consumes* its argument.  See
:ref:`in-place-updates` for the full details.

You can always obtain a unique copy of an array by using
``copy``::

  let f (a: []i32) =
    let a = copy a
    let a[0] = a[0]+1
    in a

But note that in most cases (although not all), this subverts the
purpose of using in-place updates in the first place.
