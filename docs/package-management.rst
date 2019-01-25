.. _package-management:

Package Management
==================

This document describes ``futhark pkg``, a minimalistic package
manager inspired by `vgo <https://research.swtch.com/vgo>`_.  A
Futhark package is a downloadable collection of ``.fut`` files and
little more.  There is a (not necessarily comprehensive) `list of
known packages <https://futhark-lang.org/pkgs>`_.

Basic Concepts
--------------

A package is uniquely identified with a *package path*, which is
similar to a URL, except without a protocol.  At the moment, package
paths are always links to Git repositories hosted on GitHub.  In the
future, this will become more flexible.  As an example, a package path
may be ``github.com/athas/fut-foo``.

Packages are versioned with `semantic version numbers
<https://semver.org/>`_ of the form ``X.Y.Z``.  Whenever versions are
indicated, all three digits must always be given (that is, ``1.0`` is
not a valid shorthand for ``1.0.0``).

Most ``futhark pkg`` operations involve reading and writing a *package
manifest*, which is always stored in a file called ``futhark.pkg``.
The ``futhark.pkg`` file is human-editable, but is in day-to-day use
mainly modified by ``futhark pkg`` automatically.

Using Packages
--------------

Required packages can be added by using ``futhark pkg add``, for example::

  $ futhark pkg add github.com/athas/fut-foo 0.1.0

This will create a new file ``futhark.pkg`` with the following contents:

.. code-block:: text

   require {
     github.com/athas/fut-foo 0.1.0 #d285563c25c5152b1ae80fc64de64ff2775fa733
   }

This lists one required package, with its package path, minimum
version (see :ref:`version-selection`), and the expected commit hash.
The latter is used for verification, to ensure that the contents of a
package version cannot be changed silently.

``futhark pkg`` will perform network requests to determine whether a
package of the given name and with the given version exists and fail
otherwise (but it will not check whether the package is otherwise
well-formed).  The version number can be elided, in which case
``futhark pkg`` will use the newest available version.  If the package
is already present in ``futhark.pkg``, it will simply have its version
requirement changed to the one specified in the command.  Any
dependencies of the package will *not* be added to ``futhark.pkg``,
but will still be downloaded by ``futhark pkg sync`` (see below).

Adding a package with ``futhark pkg add`` modifies ``futhark.pkg``,
but does not download the package files.  This is done with
``futhark pkg sync`` (without further options).  The contents of each
required dependency and any transitive dependencies will be stored in
a subdirectory of ``lib/`` corresponding to their package path.  As an
example::

  $ futhark pkg sync
  $ tree lib
  lib
  └── github.com
      └── athas
          └── fut-foo
              └── foo.fut

  3 directories, 1 file

**Warning:** ``futhark pkg sync`` will remove any unrecognized files or
local modifications to files in ``lib/`` (except of course the package
directory of the package path listed in ``futhark.pkg``; see
:ref:`creating-packages`).

Packages can be removed from ``futhark.pkg`` with::

  $ futhark pkg remove pkgpath

You will need to run ``futhark pkg sync`` to actually remove the files in
``lib/``.

The intended usage is that ``futhark.pkg`` is added to version
control, but ``lib/`` is not, as the contents of ``lib/`` can always
be reproduced from ``futhark.pkg``.  However, adding ``lib/`` works
just fine as well.

Importing Files from Dependencies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``futhark pkg sync`` will populate the ``lib/`` directory, but does
not interact with the compiler in any way.  The downloaded files can
be imported using the usual ``import`` mechanism (:ref:`other-files`);
for example, assuming the package contains a file ``foo.fut``::

  import "lib/github.com/athas/fut-foo/foo"

Ultimately, everything boils down to ordinary file system semantics.
This has the downside of relatively long and clumsy import paths, but
the upside of predictability.

Upgrading Dependencies
~~~~~~~~~~~~~~~~~~~~~~

The ``futhark pkg upgrade`` command will update every version
requirement in ``futhark.pkg`` to be the most recent available
version.  You still need to run ``futhark pkg sync`` to actually
retrieve the new versions.  Be careful - while upgrades are safe if
semantic versioning is followed correctly, this is not yet properly
machine-checked, so human mistakes may occur.

As an example:

.. code-block:: text

   $ cat futhark.pkg
   require {
     github.com/athas/fut-foo 0.1.0 #d285563c25c5152b1ae80fc64de64ff2775fa733
   }
   $ futhark pkg upgrade
   Upgraded github.com/athas/fut-foo 0.1.0 => 0.2.1.
   $ cat futhark.pkg
   require {
     github.com/athas/fut-foo 0.2.1 #3ddc9fc93c1d8ce560a3961e55547e5c78bd0f3e
   }
   $ futhark pkg sync
   $ tree lib
   lib
   └── github.com
       └── athas
           ├── fut-bar
           │   └── bar.fut
           └── fut-foo
               └── foo.fut

   4 directories, 2 files

Note that ``fut-foo 0.2.1`` depends on ``github.com/athas/fut-bar``,
so it was fetched by ``futhark pkg sync``.

``futhark pkg upgrade`` will *never* upgrade across a major version
number.  Due to the principle of `Semantic Import Versioning
<https://research.swtch.com/vgo-import>`_, a new major version is a
completely different package from the point of view of the package
manager.  Thus, to upgrade to a new major version, you will need to
use ``futhark pkg add`` to add the new version and ``futhark pkg
remove`` to remove the old version.  Or you can keep it around - it is
perfectly acceptable to depend on multiple major versions of the same
package, because they are really different packages.

.. _creating-packages:

Creating Packages
-----------------

A package is a directory tree (which at the moment must correspond to
a Git repository).  It *must* contain two things:

  * A file ``futhark.pkg`` at the root defining the package path and
    any required packages.

  * A *package directory* ``lib/pkg-path``, where ``pkg-path`` is the
    full package path.

The contents of the package directory is what will be made available
to users of the package.  The repository may contain other things
(tests, data files, examples, docs, other programs, etc), but these
are ignored by ``futhark pkg``.  This structure can be created
automatically by running for example::

  $ futhark pkg init github.com/sturluson/edda

Note again, no ``https://``.  The result is this ``futhark.pkg``::

  package github.com/sturluson/edda

  require {
  }

And this file hierarchy:

.. code-block:: text

   $ tree lib
   lib
   └── github.com
       └── sturluson
           └── edda

   3 directories, 0 files

Note that ``futhark pkg init`` is not necessary simply to *use*
packages, only when *creating* packages.

When creating a package, the ``.fut`` files we are writing will be
located inside the ``lib/`` directory.  If the package has its own
dependencies, whose files we would like to access, we can use
*relative imports*.  For example, assume we are creating a package
``github.com/sturluson/edda`` and we are writing a Futhark file
located at ``lib/github.com/sturluson/edda/saga.fut``.  Further, we
have a dependency on the package ``github.com/athas/foo-fut``, which
is stored in the directory ``lib/github.com/athas/foo-fut``.  We can
import a file ``lib/github.com/athas/foo-fut/foo.fut`` from
``lib/github.com/sturluson/edda/saga.fut`` with::

  import "../foo-fut/foo"

Releasing a Package
~~~~~~~~~~~~~~~~~~~

Currently, a package corresponds exactly to a GitHub repository
mirroring the package path.  A release is done by tagging an
appropriate commit with ``git tag vX.Y.Z`` and then pushing the tag to
GitHub with ``git push --tags``.  In the future, this will be
generalised to other code hosting sites and version control systems
(and possibly self-hosted tarballs).  Remember to take semantic
versioning into account - unless you bump the major version number (or
the major version is 0), the new version must be *fully compatible*
with the old.

When releasing a new package, consider getting it added to the
`central package list <https://futhark-lang.org/pkgs>`_.  See `this
page
<https://github.com/diku-dk/futhark-docbot/blob/master/README.md>`_
for details.

Incrementing the Major Version Number
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While backwards-incompatible modifications to a package are sometimes
unavoidable, it is wise to avoid them as much as possible, as they
significantly inconvenience users.  To discourage breaking
compatibility, ``futhark pkg`` tries to ensure that the package
developer feels this inconvenience as well.  In many cases, an
incompatible change can be avoided simply by adding new files to the
package rather than incompatibly changing the existing ones.

In the general case, the package path also encodes the major version
of the package, separated with a ``@``.  For example, version 5.2.1 of
a package might have the package path ``github.com/user/repo@5``.  For
major versions 0 and 1, this can be elided.  This means that multiple
(major) versions of a package are completely distinct from the point
of view of the package manager - this principle is called `Semantic
Import Versioning <https://research.swtch.com/vgo-import>`_, and is
intended to facilitate backwards compatibility of packages when new
versions are released.

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

Renaming a Package
~~~~~~~~~~~~~~~~~~

It is likely that the hosting location for a very long-lived package
will change from time to time.  Since the hosting location is embedded
into the package path itself, this causes some issues for
``futhark pkg``.

In simple cases, there is no problem.  Consider a package
``github.com/asgard/loki`` which is moved to
``github.com/utgard/loki``.  If no GitHub-level redirect is set up,
all users must update the path by which they import the package.  This
is unavoidable, unfortunately.

However, the old tagged versions, which contain a ``futhark.pkg`` that
uses the old package path, will continue to work.  This is because the
package path indicated in ``package.pkg`` merely defines the
subdirectory of ``lib/`` where the package files are to be found,
while the package path used by dependents in the ``require`` section
defines where the package files are located after ``futhark pkg
sync``.  Thus, when we import an old version of
``github.com/utgard/loki`` whose ``futhark.pkg`` defines the package
as ``github.com/asgard/loki``, the package files will be retrieved
from the ``lib/github.com/asgard/loki`` directory in the repository,
but stored at ``lib/github.com/utgard/loki`` in the local directory.

The above means that package management remains operational in simple
cases of renaming, but it is awkward when a transitive dependency is
renamed (or deleted).  The Futhark package ecosystem is sufficiently
embryonic that we have not yet developed more robust solutions.  When
such solutions are developed, they will likely involve some form of
``replace`` directive that allows transparent local renaming of
packages, as well as perhaps a central registry of package paths that
does not depend on specific source code hosts.

.. _version-selection:

Version Selection
-----------------

The package manifest ``futhark.pkg`` declares which packages the
program depends on.  Dependencies are specified as the *oldest
acceptable version* within the given major version.  Upper version
bounds are not supported, as strict adherence to semantic versioning
is assumed, so any later version with the same major version number
should work.  When ``futhark pkg sync`` calculates which version of a
given package to download, it will pick the oldest version that still
satisfies the minimum version requirements of that package in all
transitive dependencies.  This means that a version may be used that
is newer than the one indicated in ``futhark.pkg``, but only if a
dependency requires a more recent version.

Tests and Documentation for Dependencies
----------------------------------------

Package management has been designed to ensure that the normal
development tools work as expected with the contents of the ``lib/``
directory.  For example, to ensure that all dependencies do in fact
work well (or at least compile) together, run:

.. code-block:: text

   futhark test lib

Also, you can generate hyperlinked documentation for all dependencies
with:

.. code-block:: text

   futhark doc lib -o docs

The file ``docs/index.html`` can be opened in a web browser to browse
the documentation.  Prebuilt documentation is also available via the
`online package list <https://futhark-lang.org/pkgs>`_.

Safety
------

In contrast to some other package managers, ``futhark pkg`` does not
run any package-supplied code on installation, upgrade, or removal.
This means that all ``futhark pkg`` operations are in principle
completely safe (barring exploitable bugs in ``futhark pkg`` itself,
which is unlikely but not impossible).  Further, Futhark code itself
is also completely pure, so executing it cannot have any unfortunate
effects, such as `infecting all of your own packages with a worm
<https://jamie.build/how-to-build-an-npm-worm>`_.  The worst it can do
is loop infinitely, consume arbitrarily large amounts of memory, or
produce wrong results.

The exception is packages that uses ``unsafe``.  With some cleverness,
``unsafe`` can be combined with in-place updates to perform arbitrary
memory reads and writes, which can trivially lead to exploitable
behaviour.  You should not use untrusted code that employs ``unsafe``
(but the ``--safe`` compiler option may help).  However, this is not
any worse than calling external code in a conventional impure
language, which generally can perform any conceivable harmful action.
