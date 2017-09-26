#!/usr/bin/env bash

ANTIDOTE_INIT="cd antidote && make clear_background && exit"

echo "Starting antidote instances"
ssh -t jpdsousa@$1 $ANTIDOTE_INIT
ssh -t jpdsousa@$2 $ANTIDOTE_INIT
ssh -t jpdsousa@$3 $ANTIDOTE_INIT
echo "Done"