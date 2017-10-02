#!/usr/bin/env bash

echo "Resetting repo"
git reset --hard
git pull
make all

echo "Writing ips to config files..."
sed -i "s/Nodes/[\"$1\", \"$2\", \"$3\"]/g" ./config/*.config
echo "Done"

echo "Fetching self public ip..."
MY_IP=$(curl v4.ifconfig.co)
BASHO_NODE_NAME="basho@$MY_IP"
sed -i "s/BASHOIP/$MY_IP/g" ./scripts/connect_dcs.escript
echo "Done"

echo "Writing ips to escript file"
sed -i "s/Nodes/[\"$1\", \"$2\", \"$3\"]/g" ./scripts/connect_dcs.escript
echo "Done"

echo "Preparing ssh connections"
./scripts/ssh_setup.sh $1 $2 $3
echo "Done"

for f in ${1:-./config/*.config}
do
  ./scripts/ssh_antidote_start.sh $1 $2 $3
  echo "Running $f"
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