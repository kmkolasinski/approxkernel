#!/bin/bash


cd src
make clean_lib
make 
make clean
make clean C=DEBUG

cd ../tests
make 
make run
make clean
cd ..
