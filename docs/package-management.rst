.. _package-management:

Package Management
==================

This document describes ``futhark-pkg``, the Futhark package manager.
Futhark takes a very simplistic approach to package management - the
only purpose of the package manager is to download ``.fut`` files from
the Internet onto the local file system.  Actually accessing those
files is done with the usual mechanisms (:ref:`other-files`).  In
particular, ``futhark-pkg`` is not a build system, and the Futhark
compiler must still be executed as usual.  The only objective of
``futhark-pkg`` is to store files in a ``lib/`` directory; how you
access them afterwards is up to you.

``futhark-pkg`` uses a scheme based on `semantic versioning
<https://semver.org/>`_ to ensure that the same files (or "packages")
are downloaded every time, and to facilitate safe upgrades to newer
versions of a package.  The Futhark package manager derives
significant inspiration from the `vgo
<https://research.swtch.com/vgo>`_ system designed by Russ Cox.

Package Management Basics
-------------------------

A package is uniquely identified with a *package path*.  The package
path also encodes information about where to obtain the package;
typically this means it is (part of) a URL.  At the moment, package
paths are always links to Git repositories hosted on GitHub.  In the
future, this will become more flexible.  As an, a package path may be
``github.com/user/repo`` (note that there is no protocol part, so it
is not a proper URL).  The available *versions* of a package are
commits tagged with Git tags of the form ``vX.Y.Z``.  Whenever
versions are indicated, all three digits must always be given (that
is, ``1.0`` is not a valid shorthand for ``1.0.0``).  In the general
case, the package path also encodes the major version of the package,
separated with a ``@``.  For example, version 5.2.1 of a package might
have the package path ``github.com/user/repo@5``.  For major versions
0 and 1, this can be elided.  This means that multiple (major)
versions of a package are completely distinct from the point of view
of the package manager - this principle is called `Semantic Import
Versioning <https://research.swtch.com/vgo-import>`_, and is intended
to facilitate backwards compatibility of packages when new versions
are released.

Most ``futhark-pkg`` operations involve reading and writing a *package
manifest*, which is always stored in a file called ``futhark.pkg``.
The package manifest defines the name of the package as well as which
other packages it depends on.  Dependencies are specified as the
*oldest acceptable version* within the given major version.  Upper
version bounds are not supported, as strict adherence to semantic
versioning is assumed, so any later version with the same major
version number should work.  When ``futhark-pkg`` calculates which
version of a given package to download, it will pick the oldest
version that still satisfies the minimum version requirements of that
package in all transitive dependencies.  This means that a version may
be used that is newer than the one indicated in ``futhark.pkg``, but
only if a dependency requires a more recent version.

The ``futhark.pkg`` file is human-editable, but the intent is that the
subcommands provided by ``futhark-pkg`` (see below) are sufficient for
day-to-day use.

Defining a Package
------------------

A package is a directory tree (which at the moment must correspond to
a Git repository).  It *must* contain two things:

  * A file ``futhark.pkg`` at the root defining the package path and
    any required packages.

  * A *package directory* ``lib/pkg-path``, where ``pkg-path`` is the
    full package path.

The contents of the package directory is what will be made available
to users of the package.  The repository may contain other things
(tests, data files, examples, docs, other programs, etc), but these
are ignored by ``futhark-pkg``.  This structure can be created
automatically by running::

  futhark-pkg create pkgpath

Where ``pkgpath`` is the package path of the new module (such as
``github.com/sturluson/edda`` - note again, no ``https://``).

Managing Dependencies
---------------------

Required packages can be added by using::

  futhark-pkg add pkgpath X.Y.Z

This will add the indicated package to ``futhark.pkg``, if it exists.
``futhark-pkg`` will perform network requests to determine whether a
package of the given name and with the given version exists (but it
will not check whether the package is otherwise well-formed).  The
version number can be elided, in which case ``futhark-pkg`` will use
the newest available version.  If the package is already present in
``futhark.pkg``, it will simply have its version requirement changed
to the one specified in the command.

Adding a package with ``futhark-pkg add`` only modifies
``futhark.pkg``, but does not download the package files.  This is
done with ``futhark-pkg get``.  The contents of each package will be
stored in a subdirectory of ``lib/`` corresponding to their package
path.  For example, a dependency ``github.com/sturluson/edda`` will be
stored in ``lib/github.com/sturluson/edda``.

Packages can be removed from ``futhark.pkg`` with::

  futhark-pkg remove pkgpath

This will not delete any files in the ``lib/`` directory.  You will
have to do that manually.

Importing Files from Dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``futhark-pkg get`` will populate the ``lib/`` directory, but does not
interact with the compiler in any way.  The downloaded files can be
imported using the usual ``import`` mechanism (:ref:`other-files`);
for example, assuming the package contains a file ``saga.fut``::

  import "lib/github.com/sturluson/edda/saga"

This works fine when writing Futhark code that is not intended to be
imported as a package by other Futhark programs.  When writing a
reusable package, the source files will already be located inside the
``lib/`` directory (see the discussion of the *package directory*
above).  In such cases we will use *relative imports*.  For example,
assume we are defining a package ``github.com/sturluson/edda`` and we
are writing a Futhark file located at
``lib/github.com/sturluson/edda/saga.fut``.  Further, we have a
dependency on the package ``github.com/athas/foo-fut``, which is
stored in the directory ``lib/github.com/athas/foo-fut``.  We can
import a file ``foo.fut`` from ``saga.fut`` with::

  import "../foo-fut/foo"

Ultimately, everything boils down to ordinary file system semantics.
This has the downside of relatively long and clumsy file paths, but
the advantage of predictability.

Upgrading Dependencies
~~~~~~~~~~~~~~~~~~~~~~

The ``futhark-pkg upgrade`` command will update every version
requirement in ``futhark.pkg`` to be the most recent available
version.  You still need to run ``futhark-pkg get`` to actually
retrieve the new versions.  Be careful - while upgrades are safe if
semantic versioning is followed correctly, this is not yet properly
machine-checked, so human mistakes may occur.

Note that ``futhark-pkg upgrade`` will *never* upgrade across a major
version number.  Due to the principle of Semantic Import Versioning, a
new major version is a completely different package from the point of
view of the package manager.  Thus, you will need to use ``futhark-pkg
add`` to add it and ``futhark-pkg remove`` to remove the old major
version.

Releasing a Package
-------------------

Currently, a package corresponds exactly to a GitHub repository
mirroring the package path.  A release is done by tagging an
appropriate commit with ``git tag vX.Y.Z`` and then pushing the tag to
GitHub with ``git push --tags``.  In the future, this will be
generalised to other code hosting sites and version control systems
(and possibly self-hosted tarballs).  Remember to take semantic
versioning into account - unless you bump the major version number (or
the major version is 0), the new version must be *fully compatible*
with the old.

Incrementing the Major Version Number
-------------------------------------

While backwards-incompatible modifications to a package are ultimately
unavoidable, it is wise to avoid them as much as possible, as they
significantly inconvenience users.  Futher, ``futhark-pkg`` also tries
to ensure that the package developer feels this inconvenience as well,
to discourage breaking compatibility.  In many cases, an incompatible
change can be avoided simply by adding new files to the package rather
than incompatibly changing the existing ones.

If you really must increment the major version, then you will need to
change the package path in ``futhark.pkg`` to contain the new major
version preceded by ``@``.  For example,
``lib/github.com/sturluson/edda`` becomes
``lib/github.com/sturluson/edda@2``.  As a special case, this is not
necessary when moving from major version 0 to 1.  Since the package
path has changed, you will also need to rename the package directory
in ``lib/``.  This is painful and awkward, but it is less painful and
awkward than what users feel when their dependencies break
compatibility.
