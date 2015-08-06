! file fracture_output.f90
!
! This subroutine write on file the results computed in the MC simulation

SUBROUTINE fracture_output 

! Use modules

    USE mod_dynamic_allocation

    USE mod_global

! Variables declaration

    IMPLICIT NONE

    INTEGER:: k,i,j

    iunit=5

    CALL openfile (iunit,pathname='output/',filename=output_filename1,filestatus='NEW')
    !Fracture output

    WRITE (iunit,'(A,I2,A,I6,A,I6)') '% nkind= ',n_kind,'; nzone= ',n_zone,'; n_timech=',n_timech
    WRITE (iunit,'(A,E12.6,A,E12.6)') '% time end= ',time_sim_end,'; cell space (1d) = ',spece_step
    WRITE (iunit,'(8(A,I10))') '% Particles followed= ',num_particles%in, &
&	                           ', n_dead_particle: ',num_particles%dead,  &
&                              ', aborted source: ',num_particles%aborted_source,       &
&                              ', aborted red.fu: ',num_particles%aborted_rf,       &
&							   ', out E: ',num_particles%E,               &
&							   ', out W: ',num_particles%W,               &
&							   ', out N: ',num_particles%N,               &
&							   ', out S: ',num_particles%S

    WRITE(iunit,'(A)') '%  nkind | nzone | num time channels ->'

	DO k=1,n_kind
	
		DO i=1,n_zone
		
			WRITE (iunit,'(/,2I6,1X,$)') k,i
		
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
		
			WRITE (iunit,'(/,2I6,1X,$)') k,i
		
			DO j=1,n_timech
	
				WRITE (iunit,'(ES12.4,$)') var(k,i,j)
			
			END DO

		END DO

   END DO
   
	END IF

	CLOSE (unit=iunit)	

	write (*,*) 'SUBROUTINE fracture_output: OK'

END SUBROUTINE fracture_output 

