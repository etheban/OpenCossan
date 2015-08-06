!
! This subroutine generate the mesh for the MC simulation(s)


! file generate_mesh.f90
! Load the rates of transition form file
!

SUBROUTINE generate_mesh

	!	use modules

	USE mod_dynamic_allocation

	USE mod_global

! Variables Declariation 

	IMPLICIT NONE 

	INTEGER:: i,j,izxl

	CHARACTER*32:: filename,pathname

	Write (*,*) 'begin generate mesh'


!	The unit 10 is used to report input information

!	Body of the subroutine

	SELECT CASE (n_spacedim)

		CASE (1)

			filename='mesh1d.txt'   !default filename of the matrix of transition
			pathname='input/'	  !Default path of the input file

			iunit=2

			CALL openfile (iunit,pathname,filename,filestatus='UNKNOWN')
			
!			!debug
!				Write (*,*) 'n_zone=',n_zone
!				Write (*,*) 'space cell = ',spece_step
!			!end debug

			DO i=1,n_zone

!				id_zones(i)%E=idzv(i+1,FLOAT(i)*spece_step)
!				id_zones(i)%W=idzv(i-1,FLOAT(i-1)*spece_step)


				id_zones(i)%E=idzv(i+1,i*spece_step)
				id_zones(i)%W=idzv(i-1,(i-1)*spece_step)

				IF (i.eq.1.OR.i.eq.n_zone) THEN
					id_zones(i)%zonekind=0
				ELSE
					id_zones(i)%zonekind=1
				END IF

				WRITE (iunit,*) i,id_zones(i)%zonekind,id_zones(i)%E,id_zones(i)%W

			END DO

		
		CASE (2)
			filename='mesh2d.txt'   !default filename of the matrix of transition
			pathname='input/'	  !Default path of the input file

			iunit=2

			CALL openfile (iunit,pathname,filename,filestatus='UNKNOWN')

			izxl=INT((n_zone)**0.5)

			IF (izxl**2.NE.n_zone) THEN
				WRITE (*,*) 'Warning: number of zone changed from ',n_zone,' to ',izxl**2
				n_zone=izxl
			END IF
	
			WRITE (iunit,*) '% AM: 2d. zonekind, E(zone,border),W(zone,border),N(zone,border),S(zone,border)'
			
			DO j=1,izxl
				DO i=1,izxl
					izone=(j-1)*izxl+i
					id_zones(izone)%E=idzv(izone+1,FLOAT(i)*spece_step)
					id_zones(izone)%W=idzv(izone-1,FLOAT(i-1)*spece_step)
					id_zones(izone)%N=idzv(izone+izxl,FLOAT(j)*spece_step)
					id_zones(izone)%S=idzv(izone-izxl,FLOAT(j-1)*spece_step)

					IF (i.eq.1.OR.i.eq.izxl.OR.j.eq.1.OR.j.eq.izxl) THEN
						id_zones(izone)%zonekind=0
					ELSE
						id_zones(izone)%zonekind=1
					END IF
				WRITE (iunit,'(I6,1X,I2,1X,4(I6,1X,E12.3,1X))') izone,id_zones(izone)%zonekind,id_zones(izone)%E,id_zones(izone)%W,id_zones(izone)%N,id_zones(izone)%S
				END DO
			END DO

    	CASE (3)

		CASE DEFAULT

	END SELECT

	CLOSE (iunit)


END SUBROUTINE generate_mesh

