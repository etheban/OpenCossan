#!/bin/bash
#$ -S /bin/bash
#$ -cwd 
#$ -q all.q
#$ -l hostname=cossan.cfd.liv.ac.uk
#$ -e 20141215T201628_job_1.err
#$ -o 20141215T201628_job_1.out
START_DIR=`pwd` 
cd /home/ep/tmp/20141215T201628_job_1;
echo Script execution started; date
hostname
./run_Connector.sh /home/ep/workspace/OpenCossan/trunk/OpenSourceSoftware/ /usr/software/matlab/MATLAB_Compiler_Runtime/v81/
echo Script execution finished; date
mv $START_DIR/20141215T201628_job_1.err /home/ep/tmp/20141215T201628_job_1
mv $START_DIR/20141215T201628_job_1.out /home/ep/tmp/20141215T201628_job_1
