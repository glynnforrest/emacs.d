#!/usr/bin/env bash

set -e -u

if test ! -f ~/.bin/z.sh
then
    curl https://raw.githubusercontent.com/rupa/z/master/z.sh > ~/.bin/z.sh
fi
