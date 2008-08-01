#!/bin/bash

mkdir -p log
mkdir -p ebin
erlc src/dorkinator.erl -o ebin

if [ $? -ne 0 ]; then exit; fi

erl -setcookie flapjack \
    -pa /usr/local/lib/yaws/ebin /Users/cmoore/clones/dorkinator/ebin \
    -yaws embedded true -s dorkinator -sname dorkinator
