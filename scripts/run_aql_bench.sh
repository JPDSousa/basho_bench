#!/usr/bin/env bash

MY_IP=$(curl v4.ifconfig.co)
BASHO_NODE_NAME="basho@$MY_IP"

./basho_bench config/antidote_aql.config -N $BASHO_NODE_NAME -C antidote