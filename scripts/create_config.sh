#!/usr/bin/env bash

echo "Creating config..."
read -p "Name: " name
mkdir ./config/$name
read -p "Driver name: " driver_name
read -p "Token char: " token_char
read -p "Duration(minutes): " duration
read -p "Max Clients: " max_clients
echo "-- Workload configuration --"
read -p "Put: " w_put
read -p "Get: " w_get
read -p "Delete: " w_del
read -p "Select All: " w_all

clients=3
while [ $clients -lt $max_clients ]
do
w_str=$w_put"-"$w_get"-"$w_del
config_file=./config/$name/$token_char$clients"C"$w_str".config"
cat > $config_file <<- EOM
{mode, max}.
{duration, $duration}.
{concurrent, $clients}.

{driver, $driver_name}.

{key_generator, {pareto_int, 500000000}}.

{value_generator, {uniform_int, 3}}.

{aql_actors, Nodes}.

{operations, [{put, $w_put},{get, $w_get}, {delete, $w_del}, {get_all, $w_all}]}.
EOM
clients=$[$clients+10]
done
