#!/usr/bin/env bash

NUM_RUNS=20
GPU=$1
make clean
make run_ifort_benchmark

for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "RUN MULTI CORE, STEP $i"
    RUN_ID=$i ./run_ifort_benchmark
done


make clean