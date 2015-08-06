#! /bin/bash
# script to compile sparceMatrix Converter
# created by EP

NOCHECKS=$1;

if [ "$NOCHECKS" != "nochecks" ]; then
        echo "sparce Matrix Converter will be installed in the folder :" $DESTDIR;

        echo "Is it corrected? [Y/N]"

        read -e CHECK

        if [ $CHECK != "Y" ]; then 
                echo "bye !"
                exit     
        fi
fi

# go to tmp folder

tar xvzf bebop_make.tar.gz -C $TMPDIR
tar xvzf bebop_util.tar.gz  -C $TMPDIR
tar xvzf sparse_matrix_converter.tar.gz  -C $TMPDIR

cd $TMPDIR

# go to bebob_util folder
cd bebop_util

make

LIBDIR=$DESTDIR/lib;
BINDIR=$DESTDIR/bin;



if [ -d $LIBDIR ]; then

        cp *.so $LIBDIR
        
else

        mkdir -p $LIBDIR
        cp *.so $LIBDIR
        
fi
# return to the root
cd ..
# go to sparse_matrix_converter folder
cd sparse_matrix_converter
make


if [ -d $BINDIR ]; then

        cp sparse_matrix_converter $BINDIR

else

        mkdir -p $BINDIR
        cp sparse_matrix_converter $BINDIR
        
fi

# return to the root
cd ..

rm $TMPDIR/bebop_make -R
rm $TMPDIR/bebop_util -R
rm $TMPDIR/sparse_matrix_converter -R

# return to the previous folder  
cd  $CURRENTPATH

echo "Libraries copied to:" $LIBDIR
echo "Binaries copied to:" $BINDIR
echo "bye bye!"
