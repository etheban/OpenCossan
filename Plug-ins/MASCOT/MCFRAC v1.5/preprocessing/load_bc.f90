! Read boundari condition from file 
!

SUBROUTINE Load_bc


! Variables Declariation 

	USE mod_dynamic_allocation

	USE DFLIB

	USE mod_global

	IMPLICIT NONE

	INTEGER:: istat=0, iz,in,ichk

	CHARACTER*16:: filename,pathname

	filename='bc.txt'	 !default filename of the mash matrix 
	pathname='input/'	 !Default path of the input file

	iunit=22

	CALL openfile (iunit,pathname,filename,filestatus='OLD')

!	The unit 10 is used to report input information

!	Body of the subroutine

	READ (iunit,*)

	type_bc:	DO iz=1,n_bc

					READ (iunit,*,IOSTAT=istat)	ichk
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_kind		!kind of B.C.
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_particle	!Number of particle
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_pzone		!zone of b.c			
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_pkind		!particle kind 
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_tstart		!initial time of bc 
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_tstop		!final   time of bc 
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_rate		!transition rate
					READ (iunit,*,IOSTAT=istat)	boudary_condition(iz)%bc_c0		    !Contaminant concentration C0
	
					IF (istat.ne.0) CALL readerror (iunit,istat,filename)

					IF (ichk.ne.iz) THEN
						WRITE(10,*) 'WARNING in ',filename,' !!! ichk=',ichk,' izone= ',iz
					END IF

				END DO type_bc

	CLOSE (unit=iunit)

END SUBROUTINE Load_bc