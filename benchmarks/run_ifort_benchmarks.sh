#!/usr/bin/env bash

NUM_RUNS=20
GPU=$1
make clean
make run_ifort_benchmark

# Multi CORE
for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "RUN MULTI CORE, STEP $i"
    MKL_NUM_THREADS=20 RUN_ID=$i ./run_ifort_benchmark
done

# Single CORE
for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "CPU SINGLE CODE, STEP $i"
    MKL_NUM_THREADS=1 RUN_ID=$i ./run_ifort_benchmark
done

# All COREs
for (( i=0 ; i < $NUM_RUNS ; i++))
do
    echo "ALL CORES, STEP $i"
    MKL_NUM_THREADS=40 RUN_ID=$i ./run_ifort_benchmark
done


make clean