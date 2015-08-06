! openfile.f90 

! this subroutine control the opening of the file and it intercepts the possible error

SUBROUTINE openfile (iunit,pathname,filename,filestatus)
		
!	use modules

! Variables Declariation 

	IMPLICIT NONE

	INTEGER::		istat,iunit

	CHARACTER(*)::	filename,pathname,filestatus
	CHARACTER:: filenamefull

!	Body of the subroutine
	DO

	WRITE (filenamefull,'(A,1A,A)') pathname,'/',filename

		OPEN (UNIT=iunit,FILE=filenamefull,IOSTAT=istat,STATUS=filestatus)

errnum: SELECT CASE (istat)

		CASE (0)
		
			EXIT

		CASE (29) 

			WRITE (*,*) 'ERROR NUMBER:',istat

			WRITE (*, *) 'File:',filename,' NOT FOUND'

			WRITE (*, '(A)') ' Enter new name of the file: '
			READ (*, '(A)') filename

			CYCLE

		CASE (10) 	
		
			WRITE (*,*) 'ERROR NUMBER:',istat

			WRITE (*,*) 'File:',filename
			WRITE (*,'(4A)') 'already present in the ',pathname,' directory'
			WRITE (*,*) 
			WRITE (*, '(A)') ' Enter a new name of the file: '
			READ (*, '(A)') filename

			CYCLE		

		CASE DEFAULT

			WRITE (*,*) 'ERROR NUMBER:',istat

			WRITE (*,*) 'File:',filename

			STOP

		END SELECT errnum
	END DO

END SUBROUTINE openfile
