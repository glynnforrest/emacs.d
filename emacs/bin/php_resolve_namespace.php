#!/usr/bin/env php
<?php

if ($_SERVER['argc'] < 2) {
    $basename = basename(__FILE__);
    echo <<<EOF
Usage: $basename [FILE]

Suggest the namespace for a class in FILE.

The script will search upward for a composer.json with autoload configuration
that matches a segment of FILE.

EOF;
    exit(1);
}

$file = $_SERVER['argv'][1];

foreach (getAutoloadPaths($file) as $namespace => $path) {
    $pos = strpos($file, $path);
    if ($pos === false) {
        continue;
    }
    $file = substr(dirname($file), $pos + strlen($path));
    $file = trim($namespace, '\\') . $file;
    echo str_replace('/', '\\', $file);
    exit;
}

function getAutoloadPaths($file) {
    while ($file !== '/') {
        $file = rtrim(dirname($file), '/'). '/';
        $composerJson = $file.'composer.json';
        if (!file_exists($composerJson)) {
            continue;
        }
        $config = json_decode(file_get_contents($composerJson), true);
        if (!isset($config['autoload']['psr-4'])) {
            continue;
        }
        return $config['autoload']['psr-4'];
    }

    return [];
}
