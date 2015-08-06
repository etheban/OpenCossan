! Read geometries from file 
!

SUBROUTINE Load_mesh


! Variables Declariation 

	USE mod_dynamic_allocation

	USE mod_global

	IMPLICIT NONE

	INTEGER:: istat=0, iz,ichk

	CHARACTER*16:: filename,pathname

	filename='mesh.txt'	 !default filename of the mash matrix 
	pathname='input/'	 !Default path of the input file

	iunit=3

	CALL openfile (iunit,pathname,filename,filestatus='OLD')

!	The unit 10 is used to report input information

!	Body of the subroutine

	READ (iunit,*)

	SELECT CASE (n_spacedim)

		CASE (1)

		zone_1d:	DO iz=1,n_zone

					READ (iunit,*,IOSTAT=istat)				&
&					ichk,									&
&					id_zones(iz)%zonekind,					&
&					id_zones(iz)%E%izone,					&
&					id_zones(iz)%W%izone	

					IF (istat.ne.0) CALL readerror (iunit,istat,filename)

					IF (ichk.ne.iz) THEN
						WRITE(10,*) filename,' WARNING!!! ichk=',ichk,' izone= ',iz
					END IF

				END DO zone_1d

		CASE (2)

				zone_2d:	DO iz=1,n_zone

					READ (iunit,*,IOSTAT=istat)				&
&					ichk,									&
&					id_zones(iz)%zonekind,					&
&					id_zones(iz)%E%izone,					&
&					id_zones(iz)%W%izone,                   &
&					id_zones(iz)%N%izone,					&
&					id_zones(iz)%S%izone

					IF (istat.ne.0) CALL readerror (iunit,istat,filename)

					IF (ichk.ne.iz) THEN
						WRITE(10,*) 'WARNING!!! ichk=',ichk,' izone= ',iz
					END IF	

				END DO zone_2d

		CASE (3)

		zone_3d: DO iz=1,n_zone

					READ (iunit,*,IOSTAT=istat)				&
&					ichk,									&
&					id_zones(iz)%zonekind,					&
&					id_zones(iz)%E%izone,					&
&					id_zones(iz)%W%izone,                   &	
&					id_zones(iz)%N%izone,					&
&					id_zones(iz)%S%izone,                   &
&					id_zones(iz)%U%izone,					&
&					id_zones(iz)%D%izone

					IF (istat.ne.0) CALL readerror (iunit,istat,filename)

					IF (ichk.ne.iz) THEN
						WRITE(10,*) 'WARNING!!! ichk=',ichk,' izone= ',iz
					END IF

				END DO zone_3d

		CASE DEFAULT

			WRITE (10,*) 'The space dimension should be 1,2 or 3 and not equal to ',n_spacedim
			 
	END SELECT

	CLOSE (unit=iunit)

        ! compute border of th mesh

        IF (l_border) THEN
           ELSE
        END IF

END SUBROUTINE Load_mesh
