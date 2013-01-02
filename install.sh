#!/bin/sh

# Absolute path to this script
SCRIPT=`readlink -f $0`
# Absolute path this script is in
SCRIPTPATH=`dirname $SCRIPT`

link (){
    FROM="$SCRIPTPATH/$1"
    TO=$2

    printf "ln -s %s %s\n" $FROM $TO

	rm -f $TO
    ln -s $FROM $TO
}

echo -e "WARNING: This script will clobber your config files."

read -p "Install terminal stuff? (y/n) " -n 1 -r
echo -e ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
	link . ~/.dotfiles
	link zshrc.sh ~/.zshrc
	link gitconfig ~/.gitconfig
	link bashrc ~/.bashrc
fi

read -p "Install xmonad and X11 stuff? (y/n) " -n 1 -r
echo -e ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
	link xmonad ~/.xmonad
	link xmobarrc.hs ~/.xmobarrc
	link Xresources ~/.Xresources
fi
