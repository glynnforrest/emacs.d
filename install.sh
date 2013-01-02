#!/bin/sh

# Absolute path to this script
SCRIPT=`readlink -f $0`
# Absolute path this script is in
SCRIPTPATH=`dirname $SCRIPT`

link (){
    FROM="$SCRIPTPATH/$1"
    TO=$2

    printf "ln -s %s %s\n" $FROM $TO

    rm $TO
    ln -s $FROM $TO
}

link . ~/.dotfiles
link zshrc ~/.zshrc
link gitconfig ~/.gitconfig
link bashrc ~/.bashrc
link xmonad ~/.xmonad
