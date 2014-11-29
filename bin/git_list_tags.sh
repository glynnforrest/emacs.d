#!/bin/bash

# list all git tags and the commit message for all repositories in the
# current directory.

for d in *
do
    [[ ! -d "$d" ]] && continue
    [[ ! -d "$d/.git" ]] && continue
    echo "########"
    echo "$d"
    cd "$d"
    for t in `git tag`
    do
        echo -n "$t "
        git --no-pager log --oneline `git rev-parse $t` -1
    done
    cd "$OLDPWD"
done
