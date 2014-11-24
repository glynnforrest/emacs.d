#!/bin/sh

# Get a list of php classes in a given directory.
# This is a horrible, horrible abuse of pipes, but it works!

# ARGS
if [ $# -ne 1 ]
then
    echo "Usage: php_class_finder.sh [TARGET_DIR/]";
    echo "Get a list of PHP classes in TARGET_DIR";
    exit 1;
fi

find $1 -type f -name '*.php' | # find php files
    grep -v 'Test.php' | # excluding tests
    grep -v 'phpunit' | # and phpunit files
    xargs grep '^namespace[^;]*' | # that contain a namespace declaration
    sed -E 's/.*\/([[:alnum:]]+).php(.*)/\2\1/g' | # put the last portion of the file name onto the namespace
    sed 's/:namespace //' | # remove namespace
    sed 's/;/\\/' # replace semicolon with \ to join it all together.
