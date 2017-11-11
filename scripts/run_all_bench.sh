#!/usr/bin/env bash

echo "Preparing ssh connections"
./scripts/ssh_setup.sh $1 $2 $3 $4 $5
echo "Done"

echo "Writing ips to config files..."
sed -i "s/Nodes/[\"$1\"]/g" ./config/**/*.config
SED4="\"s/Nodes/[\\\"$2\\\"]/g\""
SED5="\"s/Nodes/[\\\"$3\\\"]/g\""
ssh jpdsousa@$4 "cd basho_bench && git reset --hard && git pull && sed -i $SED4 ./config/**/*.config"
ssh jpdsousa@$5 "cd basho_bench && git reset --hard && git pull && sed -i $SED5 ./config/**/*.config"
echo "Done"

echo "Fetching self public ip..."
MY_IP=$(curl v4.ifconfig.co)
BASHO_NODE_NAME="basho@$MY_IP"
sed -i "s/BASHOIP/$MY_IP/g" ./scripts/connect_dcs.escript
echo "Done"

echo "Writing ips to escript file"
sed -i "s/Nodes/[\"$1\", \"$2\", \"$3\"]/g" ./scripts/connect_dcs.escript
echo "Done"

for f in ${6:-./config/**/*.config}
do
  ./scripts/ssh_antidote_start.sh $1 $2 $3
  echo "Running $f"
  ssh jpdsousa@$4 "screen -S basho -d -m ./basho_bench/scripts/remote_basho_bench $f $4"
  ssh jpdsousa@$5 "screen -S basho -d -m ./basho_bench/scripts/remote_basho_bench $f $5"
  ./basho_bench $f -N $BASHO_NODE_NAME -C antidote
  ./scripts/ssh_antidote_kill.sh $1 $2 $3
  echo "[5] Cooling down..."
  sleep 1m
  echo "[4] Cooling down..."
  sleep 1m
  echo "[3] Cooling down..."
  sleep 1m
  echo "[2] Cooling down..."
  sleep 1m
  echo "[1] Cooling down..."
  sleep 1m
done
