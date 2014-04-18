#!/bin/zsh
for i in ~/.dotfiles/dots/.*
do
# -n for sane directory linking
ln -sfn $i ~/`basename $i`
done
