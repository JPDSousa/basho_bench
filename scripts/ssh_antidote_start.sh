#!/usr/bin/env bash

echo "Starting antidote instances"
ssh -t jpdsousa@$1 "cd antidote && make clear_background && bash"
ssh -t jpdsousa@$2 "cd antidote && make clear_background && bash"
ssh -t jpdsousa@$3 "cd antidote && make clear_background && bash"
echo "Done"