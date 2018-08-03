.. role:: ref(emphasis)

.. _futhark-pkg(1):

===========
futhark-pkg
===========

SYNOPSIS
========

futhark-pkg add PKGPATH [X.Y.Z]

futhark-pkg check

futhark-pkg create PKGPATH

futhark-pkg fmt

futhark-pkg remove PKGPATH

futhark-pkg sync

futhark-pkg upgrade

futhark-pkg versions

DESCRIPTION
===========

This tool is used to modify the package manifest (``futhark.pkg``) and
download the required packages it describes.  ``futhark-pkg`` is not a
build system; you will still need to compile your Futhark code with
the usual compilers.  The only purpose of ``futhark-pkg`` is to
download code (and perform other package management utility tasks).
This manpage is not a general introduction to package management in
Futhark; see the User's Guide for that.

The ``futhark-pkg`` subcommands will modify only two locations in the
file system (relative to the current working directory): the
``futhark.pkg`` file, and the contents of ``lib/``.  When modifying
``lib/``, ``futhark-pkg`` constructs the new version in ``lib~new/``
and backs up the old version in ``lib~old``.  If ``futhark-pkg``
should fail for any reason, you can recover the old state by moving
``lib~old`` back.  These temporary directories are erased if
``futhark-pkg`` finishes without errors.

The ``futhark-pkg sync`` and ``futhark-pkg create`` subcommands are
the only ones that actually modifies ``lib/``; the others modify only
``futhark.pkg`` and require you to manually run ``futhark-pkg sync``
afterwards.

COMMANDS
========

futhark-pkg add PKGPATH [X.Y.Z]
-------------------------------

Add the specified package of the given minimum version as a
requirement to ``futhark.pkg``.  If no version is provided, the newest
one is used.  If the package is already required in ``futhark.pkg``,
the new version requirement will replace the old one.

Note that adding a package does not automatically download it.  Run
``futhark-pkg sync`` to do that.

futhark-pkg check
-----------------

Verify that the ``futhark.pkg`` is valid, that all required packages
are available in the indicated versions.  This command does not check
that these versions contain well-formed code.  If a package path is
defined in ``futhark.pkg``, also checks that ``.fut`` files are
located at the expected location in the file system.

futhark-pkg create PKGPATH
--------------------------

Create a new ``futhark.pkg`` defining a package with the given package
path, and initially no requirements.

futhark-pkg fmt
---------------

Reformat the ``futhark.pkg`` file, while retaining any comments.

futhark-pkg remove PKGPATH
--------------------------

Remove a package from ``futhark.pkg``.  Does *not* remove it from the
``lib/`` directory.

futhark-pkg sync
----------------

Populate the ``lib/`` directory with the packages listed in
``futhark.pkg``.  **Warning**: this will delete everything in ``lib/``
that does not relate to a file listed in ``futhark.pkg``, as well as
any local modifications.

futhark-pkg upgrade
-------------------

Upgrade all package requirements in ``futhark.pkg`` to the newest
available versions.

futhark-pkg versions PKGPATH
----------------------------

Print all available versions for the given package path.

COMMIT VERSIONS
===============

It is possible to use ``futhark-pkg`` with packages that have not yet
made proper releases.  This is done via pseudoversions of the form
``0.0.0-yyyymmddhhmmss+commitid``.  The timestamp is not verified
against the actual commit.  The timestamp ensures that newer commits
take precedence if multiple packages depend on a commit version for
the same package.  If ``futhark-pkg add`` is given a package with no
releases, the most recent commit will be used.  In this case, the
timestamp is merely set to the current time.

Commit versions are awkward and fragile, and should not be relied
upon.  Issue proper releases (even experimental 0.x version) as soon
as feasible.  Released versions also always take precedence over
commit versions, since any version number will be greater than 0.0.0.

EXAMPLES
========

Create a new package that will be hosted at
``https://github.com/sturluson/edda``::

  futhark-pkg create github.com/sturluson/edda

Add a package dependency::

  futhark-pkg add github.com/sturluson/hattatal

Download the dependencies::

  futhark-pkg sync

And then you're ready to start hacking!  (Except that these packages
do not actually exist.)

BUGS
====

Since the ``lib/`` directory is populated with transitive dependencies
as well, it is possible for a package to depend unwittingly on one of
the dependencies of its dependencies, without the ``futhark.pkg`` file
reflecting this.

There is no caching of zipballs and version lists between invocations,
so the network traffic can be rather heavy.

SEE ALSO
========

futhark-test(1), futhark-bench(1)
