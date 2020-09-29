#!/usr/bin/env bash

set -e -u

wait() {
    echo
    echo "$1"
    read -p "Press enter when ready. "
}

title() {
    echo
    echo "$1"
    echo
}

is_mac () {
    test `uname` = "Darwin"
}

if test ! -f ~/.bin/z.sh
then
    title "Installing z.sh"
    curl https://raw.githubusercontent.com/rupa/z/master/z.sh > ~/.bin/z.sh
fi
