#!/usr/bin/env bash

MY_IP=$(curl v4.ifconfig.co)
BASHO_NODE_NAME="basho@$MY_IP"

for f in ./config/*.config
do
  echo "Running $f"
  ./basho_bench $f -N $BASHO_NODE_NAME -C antidote
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