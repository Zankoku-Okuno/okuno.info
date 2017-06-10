export TPG_DB=`cat config/dev/db.d/name`
export TPG_SOCK=`ls /var/run/postgresql/.*.\`cat config/dev/db.d/port\``
