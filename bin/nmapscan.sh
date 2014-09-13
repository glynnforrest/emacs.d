#!/bin/bash

if test $# -ne 1
then
    echo "nmapscan.sh";
    echo "Run a port scan on all available hosts on a subnet.";
    echo;
    echo "Usage: nmapscan.sh [IP PREFIX]";
    echo "e.g. nmapscan.sh 192.168.0";
    exit;
fi;

echo "Scanning hosts in the $1 subnet";
nmap -A -T4 $1.0-255;
