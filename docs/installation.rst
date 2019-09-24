.. _installation:

Installation
============

There are two main ways to install the Futhark compiler: using a
precompiled tarball or compiling from source.  Both methods are
discussed below.  If you are using Linux, see
:ref:`linux-installation`.  If you are using Windows, make sure to
read :ref:`windows-installation`.  If you are using macOS, read
:ref:`macos-installation`.

Futhark is also available via `Nix <https://nixos.org/nix/>`_.  If you
are using Nix, simply install the ``futhark`` derivation from Nixpkgs.

Compiling from source
---------------------

We use the the `Haskell Tool Stack`_ to handle dependencies and
compilation of the Futhark compiler, so you will need to install the
``stack`` tool.  Fortunately, the ``stack`` developers provide ample
documentation about `installing Stack`_ on a multitude of operating
systems.  If you're lucky, it may even be in your local package
repository.

You can either retrieve a `source release tarball
<https://github.com/diku-dk/futhark/releases>`_ or perform a checkout
of our Git repository::

  $ git clone https://github.com/diku-dk/futhark.git

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

The Futhark compiler and its tools will now be built.  This step
typically requires at least 8GiB of memory.  You may be able to build
it on a smaller machine by adding the ``--fast`` option, although the
resulting Futhark compiler binary will run slower.

After building, you can copy the binaries to your ``$HOME/.local/bin``
directory by running::

  $ stack install

Note that this does not install the Futhark manual pages.

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

At the moment, we build such snapshots only for a single operating
system:

Linux (x86_64)
  `futhark-nightly-linux-x86_64.tar.xz <https://futhark-lang.org/releases/futhark-nightly-linux-x86_64.tar.xz>`_

In time, we hope to make snapshots available for more platforms, but
we are limited by system availability.

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
  <https://aur.archlinux.org/packages/futhark-nightly/>`_.

Otherwise (or if the version in the package system is too old), your
best bet is to install from source or use a tarball, as described
above.

.. _`Linuxbrew`: http://linuxbrew.sh/

.. _macos-installation:

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

The Futhark compiler itself is easily installed on Windows via
``stack`` (see above).  If you are using the default Windows console,
you may need to run ``chcp 65001`` to make Unicode characters show up
correctly.

It takes a little more work to make the OpenCL and PyOpenCL backends
functional.  This guide was last updated on the 5th of May 2016, and
is for computers using 64-bit Windows along with `CUDA 7.5`_ and
Python 2.7 (`Anaconda`_ preferred).

Also `Git for Windows`_ is required for its Linux command line tools.
If you have not marked the option to add them to path, there are
instructions below how to do so. The GUI alternative to ``git``,
`Github Desktop`_ is optional and does not come with the required
tools.

.. _`CUDA 7.5`: https://developer.nvidia.com/cuda-downloads
.. _`Anaconda`: https://www.continuum.io/downloads#_windows
.. _`Git for Windows`: https://git-scm.com/download/win
.. _`Github Desktop`: https://desktop.github.com/

Setting up Futhark and OpenCL
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1) Clone the Futhark repository to your hard drive.

2) Install `Stack`_ using the 64-bit installer.  Compile the Futhark
   compiler as described in :ref:`installation`.

3) For editing environment variables it is strongly recommended that
   you install the `Rapid Environment Editor`_

4) For a Futhark compatible C/C++ compiler, that you will also need to
   install pyOpenCL later, install MingWpy. Do this using the ``pip
   install -i https://pypi.anaconda.org/carlkl/simple mingwpy``
   command.

5) Assuming you have the latest Anaconda distribution as your primary
   one, it will get installed to a place such as
   ``C:\Users\UserName\Anaconda2\share\mingwpy``. The pip installation
   will not add its bin or include directories to path.

   To do so, open the Rapid Environment Editor and add
   ``C:\Users\UserName\Anaconda2\share\mingwpy\bin`` to the system-wide
   ``PATH`` variable.

   If you have other MingW or GCC distributions, make sure MingWpy takes
   priority by moving its entry above the other distributions. You can
   also change which Python distribution is the default one using the
   same trick should you need so.

   If have done so correctly, typing ``where gcc`` in the command prompt
   should list the aforementioned MingWpy installation at the top or show
   only it.

   To finish the installation, add the
   ``C:\Users\UserName\Anaconda2\share\mingwpy\include`` to the ``CPATH``
   environment variable (note: *not* ``PATH``). Create the variable if
   necessary.

6) The header files and the .dll for OpenCL that comes with the CUDA
   7.5 distribution also need to be installed into MingWpy.  Go to
   ``C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v7.5\include``
   and copy the ``CL`` directory into the MingWpy ``include`` directory.

   Next, go to ``C:\Program Files\NVIDIA Corporation\OpenCL`` and copy
   the ``OpenCL64.dll`` file into the MingWpy ``lib`` directory (it is
   next to ``include``).

   The CUDA distribution also comes with the static ``OpenCL.lib``, but
   trying to use that one instead of the ``OpenCL64.dll`` will cause
   programs compiled with ``futhark opencl`` to crash, so ignore it
   completely.

Now you should be able to compile with ``futhark opencl`` and run
Futhark programs on the GPU.

Congratulations!

.. _`Stack`: http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows
.. _`Rapid Environment Editor`: http://www.rapidee.com/en/about

Setting up PyOpenCL
~~~~~~~~~~~~~~~~~~~

The following instructions are for how to setup the
``futhark-pyopencl`` backend.

First install Mako using ``pip install mako``.

Also install PyPNG using ``pip install pypng`` (not stricly necessary,
but some examples make use of it).

7) Clone the `PyOpenCL repository`_ to your hard drive. Do
   this instead of downloading the zip, as the zip will not contain
   some of the other repositories it links to and you will end up with
   missing header files.

8) If you have ignored the instructions and gotten Python 3.x instead
   2.7, you will have to do some extra work.

   Edit ``.\pyopencl\compyte\ndarray\gen_elemwise.py`` and
   ``.\pyopencl\compyte\ndarray\test_gpu_ndarray.py`` and convert most
   Python 2.x style print statements to Python 3 syntax. Basically wrap
   print arguments in brackets "(..)" and ignore any lines containing
   StringIO ``>>`` operator.

   Otherwise just go to the next point.

9) Go into the repo directory and from the command line execute
   ``python configure.py``.

   Edit ``siteconf.py`` to following::

     CL_TRACE = false
     CL_ENABLE_GL = false
     CL_INC_DIR = ['c:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v7.5\\include']
     CL_LIB_DIR = ['C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v7.5\\lib\\x64']
     CL_LIBNAME = ['OpenCL']
     CXXFLAGS = ['-std=c++0x']
     LDFLAGS = []

   Run the following commands::

     > python setup.py build_ext --compiler=mingw32
     > python setup.py install

If everything went in order, pyOpenCL should be installed on your machine now.

10) Lastly, Pygame needs to be installed.  Again, not stricly
    necessary, but some examples make use of it.  To do so on Windows,
    download ``pygame-1.9.2a0-cp27-none-win_amd64.whl`` from `here
    <http://www.lfd.uci.edu/~gohlke/pythonlibs/#pygame>`_. ``cp27``
    means Python 2.7 and ``win_amd64`` means 64-bit Windows.

    Go to the directory you have downloaded the file and execute ``pip
    install pygame-1.9.2a0-cp27-none-win_amd64.whl`` from the command
    line.

Now you should be able to run the `Game of Life`_ example.

11) To run the makefiles, first setup ``make`` by going to the ``bin``
    directory of MingWpy and making a copy of
    ``mingw32-make.exe``. Then simply rename ``mingw32-make –
    Copy.exe`` or similar to ``make.exe``. Now you will be able to run
    the makefiles.

    Also, if you have not selected to add the optional Linux command
    line tools to ``PATH`` during the ``Git for Windows``
    installation, add the ``C:\Program Files\Git\usr\bin`` directory
    to ``PATH`` manually now.

12) This guide has been written off memory, so if you are having
    difficulties - ask on the `issues page`_. There might be errors in
    it.

.. _`PyOpenCL repository`: https://github.com/pyopencl/pyopencl
.. _`Game of Life`: https://github.com/diku-dk/futhark-benchmarks/tree/master/misc/life
.. _`issues page`: https://github.com/diku-dk/futhark/issues
