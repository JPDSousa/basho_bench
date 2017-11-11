#!/usr/bin/env bash

cd basho_bench
./basho_bench $1 -N basho2@$2 -C antidote
