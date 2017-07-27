#!/usr/bin/env php
<?php

$args = $_SERVER['argv'];
if (count($args) < 2) {
    echo sprintf('Usage: %s <string>', basename($_SERVER['PHP_SELF'])).PHP_EOL;
    exit(1);
}

$string = $args[1];

echo array_reduce(str_split($string), function($carry, $char) {
    return $carry.'&#'.ord($char).';';
});
