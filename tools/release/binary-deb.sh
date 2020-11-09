#!/bin/sh

set -e

if [ $# != 2 ]; then
    echo "Use: $0 <repodir> <version>"
    exit 1
fi

repodir=$1
version=$2

skeletondir=$repodir/tools/release/deb-skeleton
tmpdir=$(mktemp -d)
debdir=$tmpdir/futhark-$version
deb=futhark-$version.deb

if ! [ -d "$tmpdir" ]; then
    echo "Failed to create temporary directory."
    exit 1
fi

inrepo() {
    (cd $repodir; "$@")
}

commit=$(inrepo git describe --dirty=-modified --always)

if echo "$commit" | grep -q modified; then
    echo "Refusing to package a modified repository."
    exit 1
fi

inrepo stack build
inrepo make -C docs man
binpath=$(inrepo stack path --local-install-root)/bin

umask 000 # dpkg-deb is picky about permissions.

cp -r $skeletondir $debdir

mkdir -p $debdir/usr/bin
install $binpath/* $debdir/usr/bin/

mkdir -p $debdir/usr/share/man/man1/
install -D -m 644 $repodir/docs/_build/man/*.1 $debdir/usr/share/man/man1/

sed s/VERSION/$version/ -i $debdir/DEBIAN/control

echo "Building .deb in $tmpdir..."
(cd $tmpdir && dpkg-deb --build futhark-$version)
mv $tmpdir/$deb .
rm -rf "$debdir"
echo "Created $deb."
