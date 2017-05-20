#! /bin/sh
set -e
set -x

mkdir -p deploy

# get executable
cabal clean
cabal configure && cabal build
cp dist/build/okuno-info/okuno-info deploy/
# gather sql
cp -r sql deploy/
# gather static files
cp -r static deploy/
# gather config files
cp -r config deploy/

# move to server
scp -rC deploy tsybulenko:/var/lib/okuno-info/
rm -rf deploy