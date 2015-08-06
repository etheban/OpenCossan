!  Fr_sample_transition.f90
!
!  Subroutine Fr_sample_transition
!	
!
SUBROUTINE Fr_sample_transition 

	! Use modules

	USE mod_dynamic_allocation

	USE mod_global

  ! Variables

	IMPLICIT NONE

	REAL:: rsampled, sum_rate

	INTEGER:: i

!	Initilize variables

	sum_rate=0

!	Body of the subroutine 

	rsampled=p_totrate*RAN(Iseed)


	!Cycle on the reaction 

	DO i=1,SIZE(p_move_now)

		sum_rate=sum_rate+p_move_now(i)%rate

	   	IF (sum_rate.gt.rsampled) THEN
			
			izone = p_move_now(i)%to_zone

			RETURN

		END IF

	END DO

	DO i=1,SIZE(p_reac_now) 

		sum_rate=sum_rate+p_reac_now(i)%rate

	   	IF (sum_rate.gt.rsampled) THEN
			
			ikind = p_reac_now(i)%to_kind

			RETURN

		END IF

	END DO

!	write (17,*) 'SUBROUTINE Fr_sample_transition: OK'	

END SUBROUTINE Fr_sample_transition 