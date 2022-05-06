.. _installation:

Installation
============

There are two main ways to install the Futhark compiler: using a
precompiled tarball or compiling from source.  Both methods are
discussed below.  If you are using Linux, see
:ref:`linux-installation`.  If you are using Windows, see read
:ref:`windows-installation`.  If you are using macOS, see
:ref:`macos-installation`.

Futhark is also available via `Nix <https://nixos.org/nix/>`_.  If you
are using Nix, simply install the ``futhark`` derivation from Nixpkgs.

Dependencies
------------

The Linux binaries we distribute are statically linked and should not
require any special libraries installed system-wide.

When building from source on Linux and macOS, you will need to have
the ``gmp``, ``tinfo``, and ``zlib`` libraries installed.  These are
pretty common, so you may already have them.  On Debian-like systems
(e.g. Ubuntu), use::

  sudo apt install libtinfo-dev libgmp-dev zlib1g-dev

If you install Futhark via a package manager (e.g. Homebrew, Nix, or
AUR), you shouldn't need to worry about any of this.

Actually *running* the output of the Futhark compiler may require
additional dependencies, for example an OpenCL library and GPU driver.
See the documentation for the respective compiler backends.

Compiling from source
---------------------

To compile Futhark you must first install an appropriate version of
GHC, either with [ghcup](https://www.haskell.org/ghcup/) or a package
manager.  Any version since GHC 8.10 should work.  You also need the
``cabal`` command line program, which ghcup will install for you as
well.

You then either retrieve a `source release tarball
<https://github.com/diku-dk/futhark/releases>`_ or perform a checkout
of our Git repository::

  $ git clone https://github.com/diku-dk/futhark.git

This will create a directory ``futhark``, which you must enter::

  $ cd futhark

First you must run the following command to download metadata about
Futhark's dependencies::

  $ make configure

To build the Futhark compiler and all of its dependencies, run::

  $ make build

This step typically requires at least 8GiB of memory.  This will
create files in your ``~/.cabal`` and ``~/.ghc`` directories.

After building, you can copy the binaries to your ``$HOME/.local/bin``
directory by running::

  $ make install

You can set the ``PREFIX`` environment variable to indicate a
different installation path.  Note that this does not install the
Futhark manual pages.  You can delete ``~/.cabal`` and ``~/.ghc``
after this if you wish - the ``futhark`` binary will still work.

Installing from a precompiled snapshot
--------------------------------------

Tarballs of binary releases can be `found online
<https://futhark-lang.org/releases/>`_, but are available only for
very few platforms (as of this writing, only GNU/Linux on x86_64).
See the enclosed ``README.md`` for installation instructions.

Furthermore, every day a program automatically clones the Git
repository, builds the compiler, and packages a simple tarball
containing the resulting binaries, built manpages, and a simple
``Makefile`` for installing.  The implication is that these tarballs
are not vetted in any way, nor more stable than Git HEAD at any
particular moment in time.  They are provided for users who wish to
use the most recent code, but are unable to compile Futhark
themselves.

We build such binary snapshots for the following operating systems:

**Linux (x86_64)**
  `futhark-nightly-linux-x86_64.tar.xz <https://futhark-lang.org/releases/futhark-nightly-linux-x86_64.tar.xz>`_

  You will still likely need to make a C compiler (such as GCC) available on your own.

.. _`Haskell tool stack`: http://docs.haskellstack.org/
.. _`installing Stack`: http://docs.haskellstack.org/#how-to-install

.. _linux-installation:

Installing Futhark on Linux
---------------------------

* `Linuxbrew`_ is a distribution-agnostic package manager that
  contains a formula for Futhark.  If Linuxbrew is installed (which
  does not require ``root`` access), installation is as easy as::

    $ brew install futhark

  Note that as of this writing, Linuxbrew is hampered by limited
  compute resources for building packages, so the Futhark version may
  be a bit behind.

* Arch Linux users can use a `futhark-nightly package
  <https://aur.archlinux.org/packages/futhark-nightly/>`_ or a
  `regular futhark package
  <https://aur.archlinux.org/packages/futhark>`_.

* NixOS users can install the ``futhark`` derivation.

Otherwise (or if the version in the package system is too old), your
best bet is to install from source or use a tarball, as described
above.

.. _`Linuxbrew`: http://linuxbrew.sh/

.. _macos-installation:

Using OpenCL or CUDA
~~~~~~~~~~~~~~~~~~~~

If you wish to use ``futhark opencl`` or ``futhark cuda``, you must
have the OpenCL or CUDA libraries installed, respectively.  Consult
your favourite search engine for instructions on how to do this on
your distribution.  It is usually not terribly difficult if you
already have working GPU drivers.

For OpenCL, note that there is a distinction between the general
OpenCL host library (``OpenCL.so``) that Futhark links against, and
the *Installable Client Driver* (ICD) that OpenCL uses to actually
talk to the hardware.  You will need both.  Working display drivers
for the GPU does not imply that an ICD has been installed - they are
usually in a separate package.  Consult your favourite search engine
for details.

Installing Futhark on macOS
---------------------------

Futhark is available on `Homebrew`_, and the latest release can be
installed via::

  $ brew install futhark

Or you can install the unreleased development version with::

  $ brew install --HEAD futhark

This has to compile from source, so it takes a little while (20-30
minutes is common).

macOS ships with one OpenCL platform and various devices.  One of
these devices is always the CPU, which is not fully functional, and is
never picked by Futhark by default.  You can still select it manually
with the usual mechanisms (see :ref:`executable-options`), but it is
unlikely to be able to run most Futhark programs.  Depending on the
system, there may also be one or more GPU devices, and Futhark will
simply pick the first one as always.  On multi-GPU MacBooks, this is
is the low-power integrated GPU.  It should work just fine, but you
might have better performance if you use the dedicated GPU instead.
On a Mac with an AMD GPU, this is done by passing ``-dAMD`` to the
generated Futhark executable.

.. _`Homebrew`: https://brew.sh/

.. _windows-installation:

Setting up Futhark on Windows
-----------------------------

Due to limited maintenance and testing resources, Futhark is not
directly supported on Windows.  Install `WSL
<https://docs.microsoft.com/en-us/windows/wsl/install>`_ and follow
the Linux instructions above.  The C code generated by the Futhark
compiler should work on Windows.

In the future, we may support Windows directly again.

Futhark with Nix
----------------

Futhark mostly works fine with Nix and `NixOS
<https://nixos.org/>`_, but when using OpenCL you may need to make
more packages available in your environment.  This is regardless of
whether you are using the ``futhark`` package from Nixpkgs or one you
have installed otherwise.

* On NixOS, for OpenCL, you should import ``opencl-headers`` and
  ``opencl-icd``.  You also need some form of OpenCL backend.  If you
  have an AMD GPU and use ROCm, you may also need
  ``rocm-opencl-runtime``.

* On NixOS, for CUDA (and probably also OpenCL on NVIDIA GPUs), you
  need ``cudatoolkit``.  However, ``cudatoolkit`` does not appear to
  provide ``libcuda.so`` and similar libraries.  These are instead
  provided in an ``nvidia_x11`` package that is specific to some
  kernel version, e.g. ``linuxPackages_5_4.nvidia_x11``.  You will
  need this as well.

* On macOS, for OpenCL, you need ``darwin.apple_sdk.frameworks.OpenCL``.

These can be easily made available with e.g::

  nix-shell -p opencl-headers -p opencl-icd
