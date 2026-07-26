# Flattening tests

This directory contains tests related to flattening. Because this is such a
cross-cutting part of the compiler, many other tests naturally also test
flattening indirectly. Test programs in this directory are those that directly
test properties of the flattening algorithm.

Any tests here ought to have an expected structure. When working on the
flattening pass, it may well me that some of the structure tests fail. This is
not necessarily a problem; many of the structure tests here do not truly care
about the specific number of segops (and the exceptions should be explicit), but
we have to provide some number. However, do consider whether the new structure
is actually better than the old one, or whether this is a case where the
structure doesn't really matter (because the original one was not particularly
good in the first place).

Many of the tests here will use attributes to force to go flattening a certain
way (usually full flattening). This is solely to make the code a little simpler
to read when debugging the output.
