!v.01  
!
! fr_zoning.f90
!
!  Find the zone corresponing to the current position

!
!

SUBROUTINE fr_zoning

	USE mod_global

	USE mod_dynamic_allocation

! Variables declaration

	IMPLICIT NONE

	INTEGER:: iz

	iz=1
x_zone:	DO
	  IF (rt_part%old%x.GE.id_zones(iz)%E%border) THEN
		iz=iz+1
		IF (iz.GT.n_zone) THEN

			WRITE (*,*) 'Error'
					izone=iz
			EXIT x_zone
		END IF
		CYCLE x_zone
	  ELSE
		SELECT CASE (n_spacedim)
			CASE (1)
				izone=iz
				EXIT
			CASE (2)
y_zone:		DO				
					IF (rt_part%old%y.GE.id_zones(iz)%N%border) THEN
						iz=iz+n_zone**0.5
						IF (iz.GT.n_zone) THEN
							WRITE (*,*) 'Error (vertical zones)'
							izone=iz
							EXIT x_zone
						END IF
					ELSE
						izone=iz
						EXIT x_zone
					END IF
				END DO y_zone
		END SELECT
      END IF

	END DO x_zone

	!chech the kind of zone
	IF (id_zones(izone)%zonekind.EQ.0) THEN !the particle is in a black hole zone!
	   lost_particle=.TRUE.
	   num_particles%dead=num_particles%dead+1
	END IF

	IF (l_writelog) THEN
		WRITE (99,*) 'fr_zoning'
	END IF

END SUBROUTINE fr_zoning


