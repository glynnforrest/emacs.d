#!/usr/bin/env php
<?php

$now = new \DateTime('now', new \DateTimeZone('UTC'));
$zones = [];
$maxLength = 0;

foreach (DateTimeZone::listIdentifiers() as $timezone) {
    $offset = (new DateTimeZone($timezone))->getOffset($now);

    $offsetHours = floor(abs($offset)/60/60);
    $offsetMins = (abs($offset)/60)%60;
    $offsetString = ($offset < 0 ? '-' : '+') . substr('0'.$offsetHours, -2) . substr('0'.$offsetMins, -2);

    $zones[] = [$timezone, $offsetString];

    $maxLength = max($maxLength, strlen($timezone));
}

usort($zones, function($a, $b) {
    return $a[1] <=> $b[1];
});

foreach ($zones as $zone) {
    echo sprintf('%s %s', str_pad($zone[0], $maxLength), $zone[1]) . PHP_EOL;
}
