<img src="assets/logo.svg" height="50px"/> The Futhark Programming Language
==========

[![Join the chat at https://gitter.im/futhark-lang/Lobby](https://badges.gitter.im/futhark-lang/Lobby.svg)](https://gitter.im/futhark-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![CI](https://github.com/diku-dk/futhark/workflows/CI/badge.svg)](https://github.com/diku-dk/futhark/actions)

Futhark is a purely functional data-parallel programming language in
the ML family.  Its optimising compiler is able to compile it to
typically very efficient GPU code.  The language and compiler are
developed at [DIKU](http://diku.dk) at the University of Copenhagen,
originally as part of the [HIPERFIT centre](http://hiperfit.dk).  The
language and compiler are quite stable and suitable for practical
programming.

For more information, see:

* [Installation instructions](http://futhark.readthedocs.io/en/latest/installation.html)

* [The main website](http://futhark-lang.org)

* [Parallel Programming in
  Futhark](https://futhark-book.readthedocs.io/en/latest/), an
  extensive introduction and guide

* [The Futhark User's Guide](http://futhark.readthedocs.io)

* [Documentation for the built-in prelude](https://futhark-lang.org/docs/prelude)

* [Futhark libraries](https://futhark-lang.org/pkgs/)

[![Packaging status](https://repology.org/badge/vertical-allrepos/futhark.svg)](https://repology.org/project/futhark/versions)

Hacking
=======

We try to make use of GitHub issues for organising our work.  Issues
tagged with
[good first issue](https://github.com/diku-dk/futhark/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)
do not require deep knowledge of the code base.

Testing
=======

Run `futhark test tests` to check how well we're doing.  Use `futhark
test -t` if you're in a hurry and only want to check that all the
tests type-check.
