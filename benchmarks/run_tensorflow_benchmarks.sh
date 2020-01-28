#!/usr/bin/env bash

NUM_RUNS=20
GPU=$1

# Multi CORE
for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "GPU RUN, STEP $i"
    MKL_NUM_THREADS=20 CUDA_VISIBLE_DEVICES="$GPU" python run_tensorflow_benchmark.py --name pegasus-gpu-ncpus=20 --tag run_$i
done
for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "CPU RUN, STEP $i"
    MKL_NUM_THREADS=20 CUDA_VISIBLE_DEVICES='-1' python run_tensorflow_benchmark.py --name pegasus-cpu-ncpus=20 --tag run_$i
done

# Single CORE
for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "GPU RUN, STEP $i"
    MKL_NUM_THREADS=1 CUDA_VISIBLE_DEVICES="$GPU" python run_tensorflow_benchmark.py --name pegasus-gpu-ncpus=1 --tag run_$i
done
for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "CPU RUN, STEP $i"
    MKL_NUM_THREADS=1 CUDA_VISIBLE_DEVICES='-1' python run_tensorflow_benchmark.py --name pegasus-cpu-ncpus=1 --tag run_$i
done

