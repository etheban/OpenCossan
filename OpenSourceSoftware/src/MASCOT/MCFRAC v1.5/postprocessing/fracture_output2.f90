! file fracture_output.f90
!
! This subroutine write on file the results computed in the MC simulation

SUBROUTINE fracture_output 

! Use modules

    USE mod_dynamic_allocation 

    USE mod_global

! Variables declaration

    IMPLICIT NONE

    INTEGER::	k,i,j
			 
    iunit=5

    CALL openfile (iunit,pathname='output/',filename=output_filename1,filestatus='NEW')

	!Fracture output
	
	WRITE (*,*) 'start write output file n_timech=',n_timech

	WRITE (iunit,'(A,I2,A,I4,A,I4)') '% nkind= ',n_kind,'; nzone= ',n_zone,'; n_timech=',n_timech

	WRITE(iunit,'(A)') '%  nkind | nzone | num time channels ->'
	
	
	DO k=1,n_kind
	
		DO i=1,n_zone
		
			WRITE (iunit,'(/,2I4,1X,$)') k,i
		
			DO j=1,n_timech
	
				WRITE (iunit,'(ES12.4,$)') particle_counts(k,i,j)
			
			END DO

		END DO

	END DO
											
	IF (l_variance) THEN

		WRITE(iunit,'(A)') '% &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
		WRITE(iunit,'(A)') '%  VARIANCE '
		WRITE(iunit,'(A)') '%  nkind | nzone | num time channels ->'

		DO k=1,n_kind
	
			DO i=1,n_zone
	
				WRITE (iunit,'(2(1XI4),<n_timech>(1X,ES12.4))') k,i,(var(k,i,j),j=1,n_timech)

			END DO

		END DO
	END IF

	CLOSE (unit=iunit)	

	write (*,*) 'SUBROUTINE fracture_output: OK'

END SUBROUTINE fracture_output 

