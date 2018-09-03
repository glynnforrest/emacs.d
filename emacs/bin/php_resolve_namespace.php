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
    $dir = $file;
    while ($dir !== '/') {
        $dir = rtrim(dirname($dir), '/'). '/';
        $composerJson = $dir.'composer.json';
        if (!file_exists($composerJson)) {
            continue;
        }
        $config = json_decode(file_get_contents($composerJson), true);
        $paths = array_merge(
            isset($config['autoload']['psr-4']) ? $config['autoload']['psr-4'] : [],
            isset($config['autoload-dev']['psr-4']) ? $config['autoload-dev']['psr-4'] : []
        );
        if (empty($paths)) {
            continue;
        }
        foreach ($paths as $namespace => $path) {
            $path = trim($path);
            if ($path !== '') {
                $paths[$namespace] = trim($path, '/');
                continue;
            }
            //path is empty, e.g. 'Vendor\FooBundle' => ''
            // file is src/FooBundle/Entity/Something.php
            // we want to return Vendor => 'src'
            $pieces = explode('\\', trim($namespace, '\\'));
            $lastNamespaceSegment = end($pieces);
            $pos = strpos($file, $lastNamespaceSegment);
            $paths[$namespace] = substr(dirname($file), strlen($path), $pos + strlen($lastNamespaceSegment));
        }

        return $paths;
    }

    return [];
}
