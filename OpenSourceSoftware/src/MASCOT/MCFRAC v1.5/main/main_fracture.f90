! Author Edoardo Patelli
!
! MASCOT CODE - FRACTURE STANDALONE

PROGRAM main_fracture

!  Main_fracture.f90 
!
!  MASCOT:	MONTECARLO ANALYSIS OF SUBSURFACE CONTAMINANT TRANSPORT 
!  
!  The subroutine POSTPROCESSING (Dynamic Libreries) computes the output of the code
!
!  The subroutine PREPROCESSING (Dynamic Libreries) load the input parameter for the code
!
!  The subroutine COMPUTE_MC_PARAMETERS (Dynamic Libreries) computes the transition rates 

!	use modules



   	USE mod_global
   	USE mod_sampling

! Variables Declariation 

    IMPLICIT NONE 

	 WRITE (*,*) 'MASCOT running v.1.5'

!   Preliminar operations

    CALL input_fracture_propeties  ! Read parameters form input file

	 Write (*,*) 'Allocate memory'
    
    CALL allocate_memory

    IF (l_border) THEN
		CALL generate_mesh
		WRITE (*,*) 'Mesh generated'
	ELSE
		CALL load_mesh      !Load the mesh structure from file (mesh.txt)
   		WRITE (*,*) 'Mesh loaded'
    END IF
	
	WRITE (*,*) 'Loading cdfs'
	CALL load_cdfs      !Load the cumulative ditribution functions

!T!! Debug 
!T!! test cdf sampling
!T!
!T!	Write(*,*) 'Write on file the sampled value from the length cdf'
!T!   CALL openfile (90,pathname='output/',filename='test.length',filestatus='UNKNOWN')
!T!	
!T!		DO i=1,1000000
!T!			test=f_dist_cdf ('length',2)
!T!			WRITE (90,*) test
!T!		END DO
!T!	CLOSE (90)
!T!	stop
!T!	
!T!	! test cdf sampling
!T!
!T!	Write(*,*) 'Write on file the sampled value from the velocity cdf'
!T!   CALL openfile (90,pathname='output/',filename='test.velocity',filestatus='UNKNOWN')
!T!	
!T!	DO i=1,1000000
!T!		test=f_dist_cdf ('velocity',2)
!T!		WRITE (90,*) test
!T!	END DO
!T!	CLOSE (90)
!T!	
!T!	! test cdf sampling
!T!
!T!	Write(*,*) 'Write on file the sampled value from the orientation cdf'
!T!   CALL openfile (90,pathname='output/',filename='test.orientation',filestatus='UNKNOWN')
!T!	
!T!	DO i=1,1000000
!T!		test=f_dist_cdf ('orientation',2)
!T!		WRITE (90,*) test
!T!	END DO
!T!	CLOSE (90)
!T!	
!T!   STOP

!	Start MC simulation	
   WRITE (*,*)
   WRITE (*,*) 'running MC simulation'
   CALL Fr_main

!	Write output file 
   CALL fracture_output

!	Free the memory (don't be a winzoz)
   CALL deallocate_memory

END PROGRAM main_fracture
 





