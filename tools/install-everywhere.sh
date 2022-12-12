#!/bin/sh
#
# Hacky script for installing Futhark on a bunch of servers.  Defaults
# to the servers we use at DIKU for research and teaching.

set -e

servers=$(echo \
              futhark01 \
              futhark02 \
              futhark03 \
       )
version=nightly
user=mzd885

name=futhark-$version-linux-x86_64
tarball=$name.tar.xz
tarball_url=https://futhark-lang.org/releases/$tarball

commands=$(cat <<EOF
set -e &&
dir=$(mktemp -d) &&
(cd $dir &&
wget --quiet $tarball_url &&
tar xvf $tarball &&
sudo make -C $name install);
rm -rf $dir
EOF
        )
echo $commands

for s in $servers; do
    echo "Installing on $s"
    ssh $user@$s "$commands"
done
