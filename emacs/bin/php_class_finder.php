#!/usr/bin/env php
<?php

// Get a list of php classes in a given directory.
// This is a horrible, horrible abuse of pipes, but it works!

if ($_SERVER['argc'] < 2) {
    echo <<<EOF
Usage: php_class_finder.php [TARGET_DIR/]

Get a list of PHP classes in TARGET_DIR.

A cache file is used for future runs, which can be changed with a
second argument. Pass 'refresh' to refresh the cache and 'delete' to
delete the cache. No classes will be listed when a second argument is
given.

Examples:
   php_class_finder.php ~/code/my-project
   php_class_finder.php ~/code/my-project refresh
   php_class_finder.php ~/code/my-project delete

EOF;
    exit(1);
}

$directory = $_SERVER['argv'][1];
$cache_dir = sys_get_temp_dir().'/php_class_finder_cache/';

if (!file_exists($cache_dir)) {
    mkdir($cache_dir, 0777, true);
}
$cache_file = $cache_dir . md5($directory);

if (isset($_SERVER['argv'][2]) && $_SERVER['argv'][2] === 'delete') {
    @unlink($cache_file);
    echo "Removed cache file $cache_file for $directory" . PHP_EOL;
    exit(0);
}

if (file_exists($cache_file) && !isset($_SERVER['argv'][2])) {
    echo file_get_contents($cache_file);
    exit(0);
}

//find php files
$command = "find -L $directory -type f -name '*.php'";

// excluding tests
$command .= " | grep -v 'Test.php'";

// and phpunit files
$command .= " | grep -v 'phpunit'";

// that contain a namespace declaration
$command .= " | xargs grep '^namespace[^;]*'";

// put the last portion of the file name onto the namespace
$command .= ' | sed -E "s/.*\/([[:alnum:]]+).php(.*)/\2\1/g"';

// remove namespace
$command .= ' | sed "s/:namespace //"';

// replace semicolon with a backslash to join it all together
// (I have no idea why it needs this many backslashes)
$command .= ' | sed "s/;/\\\\\\/"';

// must begin with a letter, otherwise it's junk
$command .= " | grep -E '^[a-zA-Z]'";

// remove duplicates and sort
$command .= " | sort | uniq";

// save result
$command .= "> $cache_file";

passthru($command);

if (isset($_SERVER['argv'][2])) {
    echo "Refreshed cache file $cache_file for $directory" . PHP_EOL;
    exit(0);
}

echo file_get_contents($cache_file);
