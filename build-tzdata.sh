#!/bin/bash

set -e

VER=2016j

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
cac19dded22ff80cd784641ac239358c28cce535ac1eb60b52b7e51c6e9e5fe5f795ea0252daf32c949dddb9c5b24d7e4456d7869aa2725d49c5af58b564822a  tzcode$VER.tar.gz
ce0b9958b764479fac7df6b5ba0933fc4c6d815ebccc2537c75c4be105691cd58054704eebfb50ca755d7d48504480d4925ce8836eb499ae4798e3d5503d7e0e  tzdata$VER.tar.gz
EOF

echo Unpacking... >&2
rm -rf ./tzdist
mkdir tzdist
cd tzdist
tar xzf ../tzcode$VER.tar.gz
tar xzf ../tzdata$VER.tar.gz

echo Patching... >&2
patch -p1 < $base/tzcode.patch

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
cd $base
stack build tools/

echo Creating DB.hs... >&2
cd $base
stack exec genZones tzdist/dest/etc/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
