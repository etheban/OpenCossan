!  collision_kernel.f90
!
!  Subroutine collision_kernel
!	
!
SUBROUTINE collision_kernel

!	use modules

    USE mod_global

    USE mod_dynamic_allocation

  ! Variables

    IMPLICIT NONE

! compute total cross section = scattering cross section + bla bla bla

    sigma_tot_inv=rt_part%new%fracture%length%value
    
     !for new version with non sampled sigma
	 !sigma_tot=1/fracture%length%value 
	 
	 IF (l_writelog) THEN
		WRITE (99,*) 'collision_kernel'
	END IF

END SUBROUTINE collision_kernel 
