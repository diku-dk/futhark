.. _installation:

Installation
============

There are two ways to install the Futhark compiler: using a
precompiled tarball or compiling from source.  Both methods are
discussed below.

Compiling from source
---------------------

We use the the `Haskell Tool Stack`_ to handle dependencies and
compilation of the Futhark compiler.  Thus, you will need to install
the ``stack`` tool.  Fortunately, the ``stack`` developers provide
ample documentation about `installing Stack`_ on a multitude of
operating systems.  If you're lucky, it may even be in your local
package repository.

We do not presently issue source releases of Futhark, so the only way
to compile from source is to perform a checkout of our Git
repository::

  $ git clone https://github.com/HIPERFIT/futhark.git

This will create a directory ``futhark``, which you must enter::

  $ cd futhark

To get all the prerequisites for building the Futhark compiler
(including, if necessary, the appropriate version of the Haskell
compiler), run::

  $ stack setup

Note that this will not install anything system-wide and will have no
effect outside the Futhark build directory. Now you can run the
following command to build the Futhark compiler, including all
dependencies::

  $ stack build

The Futhark compiler and its tools will now be built. You can copy
tjem to your ``$HOME/.local/bin`` directory by running::

  $ stack install

Note that this does not install the Futhark manual pages.

Installing from a precompiled snapshot
--------------------------------------

We do not yet have any proper releases as such, but every day a
program automatically clones the Git repository, builds the compiler,
and packages a simple tarball containing the resulting binaries, built
manpages, and a simple ``Makefile`` for installing.  The implication
is that these tarballs are not vetted in any way, nor more stable than
Git HEAD at any particular moment in time.  They are provided merely
for users who are unable or unwilling to compile Futhark themselves.

At the moment, we build such snapshots only for a single operating
system:

Linux (x86_64)
  `futhark-nightly-linux-x86_64.tar.xz <https://futhark-lang.org/releases/futhark-nightly-linux-x86_64.tar.xz>`_

In time, we hope to make snapshots available for more platforms, but
we are limited by system availability.  We also intend to make proper
releases once the language matures.

.. _`Haskell tool stack`: http://docs.haskellstack.org/
.. _`installing Stack`: http://docs.haskellstack.org/#how-to-install
