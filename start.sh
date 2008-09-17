#!/usr/bin/env bash

mkdir -p log
mkdir -p ebin
erlc src/dorkinator.erl -o ebin

THIS_DIR=`pwd`
HOST=`hostname`
YAWS_DIR="/usr/local/lib/yaws"

if [ $? -ne 0 ]; then exit; fi

erl -setcookie monster \
    -pa $YAWS_DIR/ebin $THIS_DIR/ebin \
    -yaws embedded true -s dorkinator -name dorkinator@elguapo.local
