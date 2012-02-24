#!/bin/bash

#really crappy implementation until I learn to do it properly.
if [ ! -h ~/.xmonad/xmonad.hs ]
then
	ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
	echo 'Made xmonad link.'
fi

if [ ! -h ~/.gitconfig ]
then
	ln -s $PWD/gitconfig ~/.gitconfig
	echo 'Made github link.'
fi
