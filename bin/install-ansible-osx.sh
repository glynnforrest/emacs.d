#!/bin/sh

ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

brew update &&
brew install python;

# pip and setuptools are installed, but upgrade them by using pip
sudo pip install pip --upgrade &&

sudo pip install ansible --upgrade
