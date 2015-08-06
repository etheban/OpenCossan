#!/bin/bash
#$ -S /bin/bash
#$ -cwd 
#$ -q pizzas64.q
#$ -e 20110429T152112.err
#$ -o 20110429T152112.out
START_DIR=`pwd` 
echo Script execution started; date
hostname
 
echo Script execution finished; date
