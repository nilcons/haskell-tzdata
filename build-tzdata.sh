#!/bin/bash

set -e

VER=2020b

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
download() {
  if [ ! -e $(basename $1) ]; then
    wget -c "$1"
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
04849f196430717962cbeedf11bbba592c304eaff5d67350c936af83dc8e8cb4cedc1c5f461c984aef05124d6c0f13a874789dff77b85a4b399faf80d75537e0  tzcode$VER.tar.gz
27ade698e61881e637ab04834633595cfbdb08fd97177e9731093165d1268a64dffa0570b5e137b9daa4374e6c6827ed01c476074ec61ec0b9a44a7f23479be9  tzdata$VER.tar.gz
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
