#!/bin/sh

set -e

if [ $# -gt 2 ]; then
    echo "Usage: $0 <path to Futhark repository> [suffix]"
    exit 1
fi

repodir=$1
suffix=$2

if [ $# -lt 2 ]; then
    suffix=-$(uname -s)-$(uname -m)-$(date +%Y-%m-%d)
    echo "Defaulting suffix to $suffix."
fi


skeletondir=$repodir/tools/release/skeleton
tmpdir=$(mktemp -d)
tarballdir=$tmpdir/futhark$suffix
tarball=futhark$suffix.tar.xz

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
bins=$(inrepo stack path --local-install-root)/bin/futhark*

cp -r $skeletondir $tarballdir
echo "$commit" > $tarballdir/commit-id

mkdir -p $tarballdir/bin
cp -r $bins $tarballdir/bin
cp $repodir/LICENSE $tarballdir/LICENSE
mkdir -p $tarballdir/share/man/man1/
cp -r $repodir/docs/_build/man/*.1 $tarballdir/share/man/man1/

echo "Building tarball in $tmpdir..."
(cd $tmpdir; tar -Jcf $tarball futhark$suffix)
mv $tmpdir/$tarball .
rm -rf "$tarballdir"
echo "Created $tarball."
