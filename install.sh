#!/bin/bash

#really crappy implementation until I learn to do it properly.
if [ ! -h ~/.xmonad/xmonad.hs ]
then
	ln xmonad.hs ~/.xmonad/xmonad.hs -s
	echo 'Made xmonad soft link.'
fi
