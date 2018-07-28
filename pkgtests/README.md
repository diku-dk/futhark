# futhark-pkg tests

This directory contains a shell script (sorry) for testing
futhark-pkg.  This is done by serially performing package management
operations, and after each operation comparing the resulting `lib`
directory against an expected `lib` directory.

It is distressingly awkward to write this using one of the normal
Haskell test frameworks.

The tests here are somewhat unstable, in that they depend on certain
remote Git repositories to have specific contents.  If they change,
you have to change this test, too.
