Put ideas for Futhark extensions/optimisations here.

Shift operation
---------------

An operation for cyclically moving the elements of an array.  Example:

    let b = shift(d, a)
    // Now b[i] = a[i-d % size(0, a)]

If we view an array 'a' as a wheel, 'shift(d,a)' moves the wheel right
by 'n' elements.  We can also view it as a bit-shift, except that the
surplus elements "falling off" the right side are added back at the
left side.

In all ways, the shape of the result of a shift is the same as the
shape of the input.  A shift operation can never fail.

The implementation is delayed, just like we envision transpose/reshape.

Crib transformations from APL
-----------------------------

Consider this: we have high-level operations such as map/reduce/filter
on arrays.  We can consider these to be transformations of the *value
space*.  Similarly, we have rearrange/reshape/replicate/etc that can
be considered *index space transformations*.  We should extend and
generalise index space transformations, like I think they are in APL!
This could allow us to get rid of abominations such as split/concat,
or at least unify them with other things.

An algebra for index space transformations, and how it relates to
traditional fusion!

APL must have this already.

`filtomap`
----------

A redomap-like construct integrating filter and map.  Semantics:

  filter f . map g ==
  filtomap(\x -> let y = g(x) in
                 if f(y) then (true, y)
                         else (false, y)

Interestingly, it also works the other way around:

  map g . filter f ==
  filtomap(\x -> if f(x) then (true, f(x))
                         else (false, dummy)

Importantly, 'filter . map . filter' can be fused to one filtomap.

It is pretty much mapMaybe from Haskell.
