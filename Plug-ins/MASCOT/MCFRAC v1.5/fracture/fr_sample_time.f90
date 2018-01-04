!  LS_sample_time.f90
!
!  Subroutine LS_sample_time
!	
!
SUBROUTINE LS_sample_time 

!	use modules

	USE mod_sampling

	USE mod_global

  ! Variables

	IMPLICIT NONE

  ! Body of the subroutine

	! Sample the time of transition from an exponential distribution 

	IF (p_totrate.eq.0) THEN

		time_new=time_sim_end

	ELSE

		time_new=time_old+f_dist_exp(p_totrate)

	END IF

!	write (17,*) 'SUBROUTINE LS_sample_time: OK'	

END SUBROUTINE LS_sample_time