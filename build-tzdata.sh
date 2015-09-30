#!/bin/bash

set -e

VER=2015f

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
wget -c http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz
wget -c http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz
wget -c http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz.asc
wget -c http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz.asc

echo Checking... >&2
gpg --verify tzdata$VER.tar.gz.asc
gpg --verify tzcode$VER.tar.gz.asc
sha512sum -c /dev/stdin <<EOF
5cfbebde8a0be57e8388a7b6d35aeba99130a72c1a41f9bbd5d5864511a473aa5e9320ab21055b4383e64b3b9d7682c0d30377ce8407619987be672436014710  tzcode$VER.tar.gz
d86e6c9e4c6d9cd5de30762723d7a90ac697feeb6dd1f04649f6a35e4e1dc638261c32066db4aeafa767ac339ccd100c38dde044ceadd61e95cb3d39c27f977e  tzdata$VER.tar.gz
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

echo Cleaning up zoneinfo root directory... >&2
cd $base/tzdist/dest/etc/zoneinfo
# We don't want these:
rm -f *.tab Factory posixrules localtime
mkdir Root
find . -maxdepth 1 -type f -exec mv '{}' Root \;
for f in Root/*; do ln -s $f .; done

echo Compiling the tool... >&2
cd $base/tools
ghc -Wall -O --make -XHaskell2010 genZones

echo Creating DB.hs... >&2
cd $base
./tools/genZones tzdist/dest/etc/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
