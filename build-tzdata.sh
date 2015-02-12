#!/bin/bash

set -e

VER=2015a

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
wget -c http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz
wget -c http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz

echo Checking... >&2
sha512sum -c /dev/stdin <<EOF
80d3bd3aeb70e21552a599c6203670fe742ca8d270c3fc430a3045d8e8a6610ebfe74941532c0791d5a99d6e4bf5fbdf62918de57d377d3fd26e7f7f786c66a4 tzcode$VER.tar.gz
ff36feb437238ba42fe67257e81ade0a2179d11f6c6449146e28e4b6553c62348b4131af75e440b4467109c9edba13e25ddaf701f2fcebb2a258b4ac9b57293c tzdata$VER.tar.gz
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
