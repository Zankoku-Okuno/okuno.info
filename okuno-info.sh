#! /bin/bash

cd /var/lib/okuno-info
source config/server/env.sh
exec ./okuno-info config/server

