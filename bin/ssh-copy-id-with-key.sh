#!/bin/bash

if test $# -ne 3
then
    SCRIPT=`basename $0`
    echo "Copy an ssh key to a host, using a given private key to login";
    echo;
    echo "Usage: $SCRIPT [PRIVATE_KEY] [HOST] [PUBLIC_KEY]";
    echo "e.g. $SCRIPT ./vagrant/machines/box/virtualbox/private_key vagrant@vagrant-box ~/.ssh/id_rsa.pub ";
    exit;
fi;

PRIVATE_KEY=$1
HOST=$2
PUBLIC_KEY=$3

echo "Copying $PUBLIC_KEY to $HOST";
cat $PUBLIC_KEY | ssh -i $PRIVATE_KEY $HOST "mkdir -p ~/.ssh && cat >>  ~/.ssh/authorized_keys"
