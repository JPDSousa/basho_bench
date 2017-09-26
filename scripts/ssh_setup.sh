#!/usr/bin/env bash

ssh-keygen -t rsa

ssh-copy-id $1
ssh-copy-id $2
ssh-copy-id $3

ssh jpdsousa@$1
ssh jpdsousa@$2
ssh jpdsousa@$3