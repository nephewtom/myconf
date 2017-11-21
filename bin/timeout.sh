#!/bin/bash
if [ -z "$1" ]; then
    TIMEOUT=60
else
    TIMEOUT=$1
    re='^[0-9]+$'
    if ! [[ $TIMEOUT =~ $re ]]; then
        echo "Error, not a valid timeout value:"$TIMEOUT
        exit 1
    fi
fi

while [ $TIMEOUT -gt 1 ]; do
    echo -n "$TIMEOUT ";
    let TIMEOUT=TIMEOUT-1
    sleep 1;
done

mplayer /usr/lib/haxe/lib/flixel/3,3,6/assets/sounds/flixel.mp3
