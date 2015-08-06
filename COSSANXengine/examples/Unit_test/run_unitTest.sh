#! /bin/bash
# script to compile ALL
# created by EP

rm ReportUnitTest.txt;

CURRENTPATH=`pwd`;

RELEASE=`lsb_release -r | awk '{print $2}'`;

DISTIBRUTION=`lsb_release -i | awk '{print $3}'`;

PROC=`uname -p`;

# echo $ARCH;

# echo $PROC;

case "$PROC" in
"i686")
ARCH=glnx86;
;;
"x86_64")
ARCH=glnxa64;
;;
*)
ARCH=glnx86;
;;
esac

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$CURRENTPATH/../../../OpenSourceSoftware/dist/$DISTIBRUTION/$RELEASE/$ARCH/lib;
export LD_LIBRARY_PATH;

/usr/site/matlab/R2010b/bin/matlab -nosplash < runAllUnitTests.m

./sendmail.sh Mechanik@uibk.ac.at Matteo.Broggi@uibk.ac.at uibk.ac.at localhost ReportUnitTest.txt "COSSAN-X: Report UnitTest"
./sendmail.sh Mechanik@uibk.ac.at Barbara.Goller@uibk.ac.at uibk.ac.at localhost ReportUnitTest.txt "COSSAN-X: Report UnitTest"
./sendmail.sh Mechanik@uibk.ac.at Murat.Panayirci@uibk.ac.at uibk.ac.at localhost ReportUnitTest.txt "COSSAN-X: Report UnitTest"
./sendmail.sh Mechanik@uibk.ac.at Pierre.Beaurepaire@uibk.ac.at uibk.ac.at localhost ReportUnitTest.txt "COSSAN-X: Report UnitTest"
./sendmail.sh Mechanik@uibk.ac.at Edoardo.Patelli@uibk.ac.at uibk.ac.at localhost ReportUnitTest.txt "COSSAN-X: Report UnitTest"

