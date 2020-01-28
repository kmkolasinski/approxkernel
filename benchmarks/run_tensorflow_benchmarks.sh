#!/usr/bin/env bash

NUM_RUNS=50
GPU=$1


for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "GPU RUN, STEP $i"
    CUDA_VISIBLE_DEVICES="$GPU" python run_tensorflow_benchmark.py --name pegasus-gpu --tag run_$i
done

for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "CPU RUN, STEP $i"
    CUDA_VISIBLE_DEVICES='-1' python run_tensorflow_benchmark.py --name pegasus-cpu --tag run_$i
done
