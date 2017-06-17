#! /bin/bash
set -e
set -x

cd /var/lib/okuno-info
source config/server/env.sh

# stop server
sudo service okuno-info stop

# backup old server
rm -rf bak
mkdir -p bak
mv okuno-info sql static config bak/

# move updated files into place
mv deploy/* .

# update database
for UPDATE in `ls sql/*-*.sql` ; do
    VERSION=`echo ${UPDATE%%-*.sql} | sed 's/^sql\/0*//'`
    if ((`psql -qtA -d $TPG_DB -c 'SELECT version FROM version;'` < $VERSION)); then
        psql -d $TPG_DB < $UPDATE
    fi
done

# start server
sudo service okuno-info start
service okuno-info status
