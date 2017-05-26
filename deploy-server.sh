#! /bin/sh
set -e
set -x

cd /var/lib/okuno-info

# stop server
sudo service okuno-info stop

# backup old server
rm -rf bak
mkdir -p bak
mv okuno-info sql static config bak/

# move updated files into place
mv deploy/* .
# start server
sudo service okuno-info start


# TODO query db version, run any sql scripts needed
echo 'DO NOT FORGET TO RUN SQL!'