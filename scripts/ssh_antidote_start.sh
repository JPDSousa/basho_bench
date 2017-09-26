#!/usr/bin/env bash

echo "Fetching self public ip..."
MY_IP=$(curl v4.ifconfig.co)
BASHO_NODE_NAME="basho@$MY_IP"
sed -i "s/BASHOIP/$MY_IP/g" ./scripts/connect_dcs.escript
echo "Done"

echo "Writing ips to escript file"
sed -i "s/Nodes/[\"$1\", \"$2\", \"$3\"]/g" ./scripts/connect_dcs.escript
echo "Done"


echo "Starting antidote instances"
ssh jpdsousa@$1 "screen -S antidote -d -m ./init_antidote.sh"
ssh jpdsousa@$2 "screen -S antidote -d -m ./init_antidote.sh"
ssh jpdsousa@$3 "screen -S antidote -d -m ./init_antidote.sh"
echo "Done"

sleep 2m
echo "Connecting data centers"
./scripts/connect_dcs.escript
echo "Done"

echo "Starting AQL instances"
ssh jpdsousa@$1 "screen -S aql -d -m ./init_aql.sh"
ssh jpdsousa@$2 "screen -S aql -d -m ./init_aql.sh"
ssh jpdsousa@$3 "screen -S aql -d -m ./init_aql.sh"
echo "Done"

echo "Starting AQL dummy instances"
ssh jpdsousa@$1 "screen -S dummy -d -m ./init_aql_dummy.sh"
ssh jpdsousa@$2 "screen -S dummy -d -m ./init_aql_dummy.sh"
ssh jpdsousa@$3 "screen -S dummy -d -m ./init_aql_dummy.sh"
echo "Done"