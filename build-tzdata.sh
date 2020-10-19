#!/bin/bash

set -e

VER=2020c

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
download() {
  if [ ! -e $(basename "$1") ]; then
    wget "$1"
  fi
}

download http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz
download http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz
download http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz.asc
download http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz.asc

echo Checking... >&2
gpg --verify tzdata$VER.tar.gz.asc
gpg --verify tzcode$VER.tar.gz.asc

sha512sum tzcode$VER.tar.gz tzdata$VER.tar.gz

sha512sum -c /dev/stdin <<EOF
c77fa69d2a005ba7cff602b2267983fd01613f81385bc13c90b9581d69fb0ac73491641cac81e0e5d7dd00ed120c45103859902c2d10da9d25c98b33354f88f7  tzcode$VER.tar.gz
bbd66fe236ba0949261cb238bfed454c03b4500b239dc38f1b8fef8d229136f5964c1a8386fe54484e4e5e34a3c28a7b66ee7374ff7e0dd07865d78fc53bf96c  tzdata$VER.tar.gz
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
mv tzdist/dest/usr/share/zoneinfo tzdata
cd tzdata
find . -type f -name '[A-Z]*' -exec mv '{}' '{}.zone' \;

echo Patching for symlinked compilation... >&2
cd $base/tzdist
patch -p1 < $base/tzcode.patch

echo Building symlinked zoneinfo for compilation... >&2
make clean
make TOPDIR=$base/tzdist/dest CFLAGS=-DHAVE_LINK=0 install

echo Cleaning up zoneinfo root directory... >&2
cd $base/tzdist/dest/usr/share/zoneinfo
# We don't want these:
rm -f *.tab Factory posixrules localtime leapseconds tzdata.zi
mkdir Root
find . -maxdepth 1 -type f -exec mv '{}' Root \;
for f in Root/*; do ln -s $f .; done

if [ "x$USE_CABAL" = "xYES" ]; then
  echo Compiling the tool... >&2
  cd $base
  cabal new-build genZones

  echo Creating DB.hs... >&2
  cd $base
  cabal new-run genZones -- tzdist/dest/usr/share/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
else
  echo Compiling the tool... >&2
  cd $base
  stack build tools/

  echo Creating DB.hs... >&2
  cd $base
  stack exec genZones tzdist/dest/usr/share/zoneinfo/ Data/Time/Zones/DB.hs.template Data/Time/Zones/DB.hs
fi
