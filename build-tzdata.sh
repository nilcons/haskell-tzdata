#!/bin/bash

set -e

VER=2014j

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
wget -c http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz
wget -c http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz

echo Checking... >&2
sha256sum -c /dev/stdin <<EOF
7fd46125464856309fc81fe85a67a61de862b8ab884ce8ca82051f5fa308ede2  tzcode$VER.tar.gz
a2d870320694d40535df822ac8074dc629a90e92abafa5d3373314f78ddc0e0d  tzdata$VER.tar.gz
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

echo Compiling the tool... >&2
cd $base/tools
ghc -Wall -O --make -XHaskell2010 genZones

echo Creating DB.hs... >&2
cd $base
./tools/genZones tzdist/dest/etc/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
