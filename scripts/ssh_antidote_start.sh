#!/usr/bin/env bash

ANTIDOTE_INIT="screen -S backup -d -m ./init_antidote.sh"

echo "Starting antidote instances"
ssh jpdsousa@$1 $ANTIDOTE_INIT
ssh jpdsousa@$2 $ANTIDOTE_INIT
ssh jpdsousa@$3 $ANTIDOTE_INIT
echo "Done"