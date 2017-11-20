#!/usr/bin/env bash

git reset --hard
echo "Preparing ssh connections"
./scripts/ssh_setup.sh $1 $2 $3 $5 $6
echo "Done"

echo "Writing ips to config files..."
sed -i "s/Nodes/[\"$1\"]/g" ./config/**/*.config
SED5="\"s/Nodes/[\\\"$2\\\"]/g\""
SED6="\"s/Nodes/[\\\"$3\\\"]/g\""
ssh jpdsousa@$5 "cd basho_bench && git reset --hard && git pull && sed -i $SED5 ./config/**/*.config"
ssh jpdsousa@$6 "cd basho_bench && git reset --hard && git pull && sed -i $SED6 ./config/**/*.config"
echo "Done"

BASHO_NODE_NAME="basho@$4"
sed -i "s/BASHOIP/$4/g" ./scripts/connect_dcs.escript
echo "Done"

echo "Writing ips to escript file"
sed -i "s/Nodes/[\"$1\", \"$2\", \"$3\"]/g" ./scripts/connect_dcs.escript
echo "Done"

for f in ${7:-./config/**/*.config}
do
  ./scripts/ssh_antidote_start.sh $1 $2 $3
  echo "Running $f"
  ssh jpdsousa@$5 "epmd -daemon && cd basho_bench && screen -S basho -d -m ./basho_bench $f -N basho@$5 -C antidote"
  ssh jpdsousa@$6 "epmd -daemon && cd basho_bench && screen -S basho -d -m ./basho_bench $f -N basho@$6 -C antidote"
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
