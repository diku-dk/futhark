# Flattening tests

This directory contains tests related to flattening. Because this is such a
cross-cutting part of the compiler, many other tests naturally also test
flattening indirectly. Test programs in this directory are those that directly
test properties of the flattening algorithm. In particular, any tests here ought
to have an expected structure. When working on the flattening pass, it may well
me that some of the structure tests fail. This is not necessarily a problem;
many of the structure tests here do not truly care about the specific number of
segops (and the exceptions should be explicit), but we have to provide some
number.
