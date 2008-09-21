#!/usr/bin/env bash

THIS_DIR=`pwd`
# "include" should be underneath this directory.
YAWS_DIR="/opt/local/lib/yaws"
# hostname and hostname -f aren't always the best
# option especially if you want to run more than
# one of these at the same time.
NAME="dorkinator@elguapo.local"

mkdir -p log
mkdir -p ebin

OPTIONS="-pa $YAWS_DIR -pa $YAWS_DIR/include -pa $YAWS_DIR/ebin $THIS_DIR/ebin"

erl -make

if [ $? -ne 0 ]; then exit; fi

erl -setcookie monster \
    -pa $YAWS_DIR -pa $YAWS_DIR/include -pa $YAWS_DIR/ebin $THIS_DIR/ebin \
    -yaws embedded true -s dorkinator -name $NAME

