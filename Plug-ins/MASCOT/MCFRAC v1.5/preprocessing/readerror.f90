! readerror.f90 

! this subroutine control the opening of the file and it intercepts the possible error

SUBROUTINE readerror (iunit,istat,filename)
		
!	use modules

! Variables Declariation 

	IMPLICIT NONE

	INTEGER::		istat,iunit

	CHARACTER*16::	filename

!	Body of the subroutine

	WRITE (*,*) 'ERROR!!!!!'

	SELECT CASE (istat)

		CASE (-1)

			WRITE (*,*) 'End of file found'

		CASE (59)

			WRITE (*,*) 'I/O error: bad data'

		CASE DEFAULT

			WRITE (*,*) 'ERROR NUMBER:',istat

		END SELECT

		WRITE (*, *) 'File: ',filename
		WRITE (*, *) 'Unit: ',iunit
		STOP


END SUBROUTINE readerror
