#!/bin/bash

function create_link {
	source="${PWD}/$1"
    target="${HOME}/${1/_/.}"

	if [ -f ${target} ]; then
		mv ${target} ${target}.bak
		echo "Moving existing $target to $target.bak"
	fi

	ln -sf ${source} ${target}
	echo "Linking $source to $target"

}

for i in _*
do
	create_link $i
done
