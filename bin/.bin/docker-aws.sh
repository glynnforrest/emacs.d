#!/bin/bash

set -e

docker build -t aws -f ~/code/github.com/glynnforrest/dotfiles/dockerfiles/aws.Dockerfile .

(docker stop aws && docker rm aws) || true

docker run -ti --rm --name aws -d --privileged aws

docker exec -ti aws /bin/bash

