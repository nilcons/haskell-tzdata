#!/bin/bash

set -e

VER=2018g

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

sha512sum tzcode$VER.tar.gz tzdata$VER.tar.gz

sha512sum -c /dev/stdin <<EOF
58f89b7323bfe795c5f13039f7527d18b15c9e37fce6e9fa1a402ce2689bf5c772cf1ffb86f23309814a563f9f429da472df1229818b07b1e04f16bdedb21484  tzcode$VER.tar.gz
92e9bbd61f51be8f2cf7ec9491691e5e2f97803914dbad77b7fb8b6600ed68fc3b98450fc808bb2d4c6c835df5f9eb7bf4529d059d9b1370f2ab4c12e7f1adfa  tzdata$VER.tar.gz
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
mv tzdist/dest/usr/share/zoneinfo tzdata
cd tzdata
find . -type f -name '[A-Z]*' -exec mv '{}' '{}.zone' \;

echo Building symlinked zoneinfo for compilation... >&2
cd $base/tzdist
make clean
make TOPDIR=$base/tzdist/dest CFLAGS=-DHAVE_LINK=0 install

echo Cleaning up zoneinfo root directory... >&2
cd $base/tzdist/dest/usr/share/zoneinfo
# We don't want these:
rm -f *.tab Factory posixrules localtime leapseconds tzdata.zi
mkdir Root
find . -maxdepth 1 -type f -exec mv '{}' Root \;
for f in Root/*; do ln -s $f .; done

echo Compiling the tool... >&2
cd $base
stack build tools/

echo Creating DB.hs... >&2
cd $base
stack exec genZones tzdist/dest/usr/share/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
