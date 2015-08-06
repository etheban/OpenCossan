#!/bin/bash
#$ -S /bin/bash
#$ -cwd 
#$ -q pizzas64.q
#$ -e 20110829T164846_sfem_1.err
#$ -o 20110829T164846_sfem_1.out
START_DIR=`pwd` 
mkdir -p /tmp/20110829T164846_sfem_1;
cp /home/ep/20110829T164846_sfem_1 /tmp/ -R;
cd /tmp/20110829T164846_sfem_1;
echo Script execution started; date
hostname
/usr/site/bin/abq673 interactive ask_delete=off job=nominal.inp  
echo Script execution finished; date
mv $START_DIR/20110829T164846_sfem_1.err /tmp/20110829T164846_sfem_1
mv $START_DIR/20110829T164846_sfem_1.out /tmp/20110829T164846_sfem_1
cp /tmp/20110829T164846_sfem_1 /home/ep// -R;
rm /tmp/20110829T164846_sfem_1 -Rf 
