#!/bin/sh

amixer | grep Master -A 4 | grep '\[on\]' > /dev/null

RETVAL=$?

[ $RETVAL -eq 0 ] && echo $(amixer | grep Master -A 4 | grep -Eo '[0-9]+%')
[ $RETVAL -ne 0 ] && echo Off

exit 0
