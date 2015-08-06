! v.01  - EDOARDO PATELLI
! fr_beta1d.f90
!
!  Subroutine 

SUBROUTINE fr_beta1d

	USE mod_global
	USE mod_dynamic_allocation

! Variables declaration

	IMPLICIT NONE
	
	REAL:: dt_exit

	IF (rt_part%new%x.gt.beta%x) THEN
		!compute the exit time 
		dt_exit=(beta%x-rt_part%old%x)/(mu*rt_part%old%fracture%velocity%value)
		!update beta
		beta%value=beta%value+dt_exit/rt_part%old%fracture%aperture%value
		beta%bfp=.TRUE.
		WRITE (9,*) beta%value
	ELSE 
		beta%value=beta%value+(rt_part%new%time-rt_part%old%time)/rt_part%old%fracture%aperture%value
	END IF
	
	IF (l_writelog) THEN
		WRITE (99,*) 'fr_beta1d'
	END IF

END SUBROUTINE fr_beta1d

SUBROUTINE fr_beta2d

	USE mod_global
	USE mod_dynamic_allocation

! Variables declaration

	IMPLICIT NONE
	
	REAL:: dt_exit

	IF (rt_part%new%x.gt.beta%x) THEN
		!compute the exit time 
		dt_exit=(beta%x-rt_part%old%x)/(mu*rt_part%old%fracture%velocity%value)
		!update beta
		beta%value=beta%value+dt_exit/rt_part%old%fracture%aperture%value
		beta%bfp=.TRUE.
		WRITE (9,*) beta%value
	ELSE 
		beta%value=beta%value+(rt_part%new%time-rt_part%old%time)/rt_part%old%fracture%aperture%value
	END IF

	IF (rt_part%new%y.gt.beta%y) THEN
		!compute the exit time 
		dt_exit=(beta%y-rt_part%old%y)/(nu*rt_part%old%fracture%velocity%value)
		!update beta
		beta%value=beta%value+dt_exit/rt_part%old%fracture%aperture%value
		beta%bfp=.TRUE.
		WRITE (9,*) beta%value
	ELSE 
		beta%value=beta%value+(rt_part%new%time-rt_part%old%time)/rt_part%old%fracture%aperture%value
	END IF
	
	IF (l_writelog) THEN
		WRITE (99,*) 'fr_beta2d'
	END IF

END SUBROUTINE fr_beta2d


