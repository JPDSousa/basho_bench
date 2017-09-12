#!/usr/bin/env bash

sed -i "s/Nodes/[\"$1\", \"$2\", \"$3\"]/g" ./config/*.config