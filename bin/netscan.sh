#!/bin/bash

if test $# -ne 1
then
    echo "netscan.sh";
    echo "Scan for hosts on a subnet.";
    echo;
    echo "Usage: netscan.sh [IP PREFIX]";
    echo "e.g. netscan.sh 192.168.0";
    exit;
fi;

echo "Checking for hosts in the $1 subnet";
nmap -sn $1.0-255;
