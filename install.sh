#!/bin/bash

function create_link {
	source="${PWD}/$1"
    target="${HOME}/${1/_/.}"

    if [ ! -e "${target}" ]; then
		ln -s ${source} ${target}
		echo "Linking $source to $target"
    fi

}

for i in _*
do
	create_link $i
done
