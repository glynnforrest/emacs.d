#!/bin/bash

function create_link {
	source="${PWD}/$1"
    target="${HOME}/${1/_/.}"

	ln -sf ${source} ${target}
	echo "Linking $source to $target"

}

for i in _*
do
	create_link $i
done
