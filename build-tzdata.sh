#!/bin/bash

set -e

VER=2014e

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
wget -c http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz
wget -c http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz

echo Checking... >&2
sha256sum -c /dev/stdin <<EOF
43ebc426ab4911e222a4487f24289adfd2bc5eb09363a5e77cdabf56374a8c1c  tzcode$VER.tar.gz
08e243a79051f838d86ac1311a78b74047951302d448024e823b5b7aa71f86c5  tzdata$VER.tar.gz
EOF

echo Unpacking... >&2
rm -rf ./tzdist
mkdir tzdist
cd tzdist
tar xzf ../tzcode$VER.tar.gz
tar xzf ../tzdata$VER.tar.gz

echo Building... >&2
make TOPDIR=$base/tzdist/dest install

echo Renaming... >&2
cd $base
rm -rf tzdata
mv tzdist/dest/etc/zoneinfo tzdata
cd tzdata
find . -type f -name '[A-Z]*' -exec mv '{}' '{}.zone' \;
rm localtime posixrules

echo Building symlinked zoneinfo for compilation... >&2
cd $base/tzdist
make clean
make TOPDIR=$base/tzdist/dest CFLAGS=-DHAVE_LINK=0 install
zdir=$base/tzdist/dest/etc/zoneinfo
# We don't want these:
rm -f $zdir/* || true
rm -rf $zdir/Etc

echo Compiling the tool... >&2
cd $base/tools
ghc -Wall -O --make -XHaskell2010 genZones

echo Creating DB.hs... >&2
cd $base
./tools/genZones tzdist/dest/etc/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
