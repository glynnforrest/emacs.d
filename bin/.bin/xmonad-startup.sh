#!/bin/bash

# Set wallpaper
feh --bg-fill ~/Pictures/background

# Start a blank tmux for urxvt to connect to
tmux new-session

# Power manager
xfce4-power-manager

# Add some style
if [ -z "$(pgrep xcompmgr)" ] ; then
	xcompmgr -cCfF -t -8 -l -8 -o .2 -D 5 &
fi

if [ -z "$(pgrep emacs)" ] ; then
	emacs -daemon
fi
