#!/usr/bin/env bash

echo "Starting antidote instances"
ssh jpdsousa@$1 "screen -S backup -d -m ./init_antidote.sh"
ssh jpdsousa@$2 "screen -S backup -d -m ./init_antidote.sh"
ssh jpdsousa@$3 "screen -S backup -d -m ./init_antidote.sh"
echo "Done"