export TPG_DB=`cat config/server/db.d/name`
export TPG_SOCK=`ls /var/run/postgresql/.*.\`cat config/server/db.d/port\``
