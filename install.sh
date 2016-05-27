#!/usr/bin/env bash

for i in .tmux.conf .vimrc .zshrc
do
    ln -sfv `realpath $i` ~/$i
done
