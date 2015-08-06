#!/bin/bash
#$ -S /bin/bash
#$ -cwd 
#$ -q pizzas64.q
#$ -e 20110829T164846_sfem_2.err
#$ -o 20110829T164846_sfem_2.out
START_DIR=`pwd` 
mkdir -p /tmp/20110829T164846_sfem_2;
cp /home/ep/20110829T164846_sfem_2 /tmp/ -R;
cd /tmp/20110829T164846_sfem_2;
echo Script execution started; date
hostname
/usr/site/bin/abq673 interactive ask_delete=off job=positive_perturbed_RV1.inp  
echo Script execution finished; date
mv $START_DIR/20110829T164846_sfem_2.err /tmp/20110829T164846_sfem_2
mv $START_DIR/20110829T164846_sfem_2.out /tmp/20110829T164846_sfem_2
cp /tmp/20110829T164846_sfem_2 /home/ep// -R;
rm /tmp/20110829T164846_sfem_2 -Rf 
