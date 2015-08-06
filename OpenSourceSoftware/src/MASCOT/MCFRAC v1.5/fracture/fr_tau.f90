! v.1  - EDOARDO PATELLI
! fr_tau1d.f90
!
!  Subroutine 

SUBROUTINE fr_tau1d

	USE mod_global
	USE mod_dynamic_allocation

! Variables declaration

	IMPLICIT NONE
	
	REAL:: dt_exit

	IF (rt_part%new%x.gt.tau%x) THEN
		!compute the exit time 
		dt_exit=(tau%x-rt_part%old%x)/(mu*rt_part%old%fracture%velocity%value)
		!compute the exit y-position
		tau%y=rt_part%old%y+(nu*rt_part%old%fracture%velocity%value)*dt_exit
		!update tau
		tau%value=rt_part%old%time+dt_exit
		tau%bfp=.TRUE.
		WRITE (10,*) tau%value,tau%x,tau%y
	END IF
	
	IF (l_writelog) THEN
		WRITE (99,*) 'fr_tau1d'
	END IF

END SUBROUTINE fr_tau1d

SUBROUTINE fr_tau2d

	USE mod_global
	USE mod_dynamic_allocation

! Variables declaration

	IMPLICIT NONE
	
	REAL:: dt_exit

	IF (rt_part%new%x.gt.tau%x) THEN
		!compute the exit time 
		dt_exit=(tau%x-rt_part%old%x)/(mu*rt_part%old%fracture%velocity%value)
		!compute the exit y-position
		tau%y=rt_part%old%y+(nu*rt_part%old%fracture%velocity%value)*dt_exit
		!update tau
		tau%value=rt_part%old%time+dt_exit
		tau%bfp=.TRUE.
		WRITE (10,*) tau%value,tau%x,tau%y
	END IF
	
	IF (l_writelog) THEN
		WRITE (99,*) 'fr_tau2d'
	END IF

END SUBROUTINE fr_tau2d

