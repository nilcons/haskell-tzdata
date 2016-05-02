#!/bin/bash

set -e

VER=2016d

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
c6f6259a78fadaab293be0a4123c226d1a327588639cfa8dadb5a74bd58552892a0c87cfd3a33888f886f51aff34465c89505f0892e6bcbe24247a9160e7328c  tzcode$VER.tar.gz
f1beb1793c4c7d18f2dadaf4a928b1476f66b400bda0c87b06155c0dd1c4b4a26bb2f37dc17a3676a2bbe9c1e71a5d8b27a171c797a86464b0bc0d13abfb2f99  tzdata$VER.tar.gz
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
cd $base
stack build tools/

echo Creating DB.hs... >&2
cd $base
stack exec genZones tzdist/dest/etc/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
