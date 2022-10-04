#!/usr/bin/env bash

set -e -u

wait() {
    echo
    echo "$1"
    read -p "Press enter when ready. "
}

# Install brew
# Install chrome

wait "* Open Google Chrome
* Login to Bitwarden
* Get your SSH key and install to ~/.ssh/
"

mkdir -p ~/code/github.com/glynnforrest/
cd ~/code/github.com/glynnforrest/

if test ! -d dotfiles
then
    echo "Cloning dotfiles..."
    git clone git@github.com:glynnforrest/dotfiles.git
fi

if test ! -d private-dotfiles
then
    echo "Cloning private dotfiles..."
    git clone ssh://git@glynnforrest.com:404/~/projects/private-dotfiles
fi
