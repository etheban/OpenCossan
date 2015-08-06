! file load_cdfs.f90
! Load the rates of transition form file
!

SUBROUTINE 	load_cdfs 

	!	use modules

	USE mod_dynamic_allocation

	USE mod_global

! Variables Declariation 

	IMPLICIT NONE 

	INTEGER:: istat=0, i,j,l,m

	CHARACTER*32:: filename,pathname,chlog

	pathname='input/'	  !Default path of the input file
	iunit=2

!	Read the cdf of the velocity from file

	IF (fr_property%velocity%dist_kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%velocity%filename),filestatus='OLD')		

		DO l=1,SIZE(cdf_velocity, DIM = 3)
			DO m=1,SIZE(cdf_velocity, DIM = 4)
				READ (iunit,'(A32)') chlog	!skip the intestation string
				IF (l_writelog) WRITE (99,*) chlog
				DO i=1,SIZE(cdf_velocity, DIM = 1)
				!the first column rapresents the x'value
					READ (iunit,*,IOSTAT=istat) (cdf_velocity(i,j,l,m),j=1,SIZE(cdf_velocity, DIM = 2))				
					IF (istat.ne.0) CALL readerror (iunit,istat,filename)
				END DO
			END DO
		END DO

		CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'cdf_velocity read '
		END IF

	END IF

!	Read the cdf of the orientation from file

	IF (fr_property%orientation%dist_kind.EQ.'cdf') THEN
		CALL openfile (iunit,TRIM(pathname),TRIM(cdf%orientation%filename),filestatus='OLD')		
 
		DO l=1,SIZE(cdf_orientation, DIM = 3)
			DO m=1,SIZE(cdf_orientation, DIM = 4)
				READ (iunit,'(A32)') chlog	!skip the intestation string
				IF (l_writelog) WRITE (99,*) chlog
				DO i=1,SIZE(cdf_orientation, DIM = 1)
				!the first column rapresents the x'value
					READ (iunit,*,IOSTAT=istat) (cdf_orientation(i,j,l,m),j=1,SIZE(cdf_orientation, DIM = 2))				
					IF (istat.ne.0) CALL readerror (iunit,istat,filename)
				END DO
			END DO
		END DO

			CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'cdf_orientation read '
		END IF

	END IF

!	Read the cdf of the length from file

	IF (fr_property%length%dist_kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%length%filename),filestatus='OLD')		
		DO l=1,SIZE(cdf_length, DIM = 3)
			DO m=1,SIZE(cdf_length, DIM = 4)
				READ (iunit,'(A32)') chlog
				IF (l_writelog) WRITE (99,*) chlog
				DO i=1,SIZE(cdf_length, DIM = 1)
				!the first column rapresents the x'value
					READ (iunit,*,IOSTAT=istat) (cdf_length(i,j,l,m),j=1,SIZE(cdf_length, DIM = 2))				
					IF (istat.ne.0) CALL readerror (iunit,istat,filename)
				END DO
			END DO
		END DO

			CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'cdf_length (cdf) read '
		END IF
	ELSEIF (fr_property%length%dist_kind.EQ.'lmv') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%length%filename),filestatus='OLD')		
		READ (iunit,'(A32)') chlog	!skip the intestation string
		IF (l_writelog) WRITE (99,*) chlog

		DO i=1,SIZE(cdf_length, DIM = 3)
			!the first column rapresents the x'value
				READ (iunit,*,IOSTAT=istat) (cdf_length(1,1,i,j),j=1,SIZE(cdf_length, DIM = 4))				
			IF (istat.ne.0) CALL readerror (iunit,istat,filename)
		END DO

			CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'cdf_length (lmv) read '
		END IF

	END IF

	!	Read the cdf of the aperture from file

	IF (fr_property%aperture%dist_kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%aperture%filename),filestatus='OLD')		

		DO l=1,SIZE(cdf_aperture, DIM = 3)
			DO m=1,SIZE(cdf_aperture, DIM = 4)
				READ (iunit,'(A32)') chlog	!skip the intestation string
				IF (l_writelog) WRITE (99,*) chlog
				DO i=1,SIZE(cdf_aperture, DIM = 1)
				!the first column rapresents the x'value
					READ (iunit,*,IOSTAT=istat) (cdf_aperture(i,j,l,m),j=1,SIZE(cdf_aperture, DIM = 2))				
					IF (istat.ne.0) CALL readerror (iunit,istat,filename)
				END DO
			END DO
		END DO

		CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'cdf_aperture read '
		END IF

	END IF

	!	Read the cdf of the source velocity from file

	IF (source%velocity%kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%source_vel%filename),filestatus='OLD')		
			READ (iunit,'(A32)') chlog	!skip the intestation string
			IF (l_writelog) WRITE (99,*) chlog

cyclesv:	DO i=1,cdf%source_vel%nbins

			!the first column rapresents the x'value

				READ (iunit,*,IOSTAT=istat)	(source_velocity(i,j),j=1,SIZE(source_velocity, DIM = 2))				
	
				IF (istat.ne.0) CALL readerror (iunit,istat,filename)

			END DO cyclesv

			CLOSE (iunit)

			
		IF (l_writelog) THEN
			WRITE (99,*) 'source_velocity read '
		END IF

	END IF

		!	Read the cdf of the source orientation from file

	IF (source%orientation%kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%source_ori%filename),filestatus='OLD')		
			READ (iunit,'(A32)') chlog	!skip the intestation string
			IF (l_writelog) WRITE (99,*) chlog

cycleso:	DO i=1,cdf%source_ori%nbins

			!the first column rapresents the x'value

				READ (iunit,*,IOSTAT=istat)	(source_orientation(i,j),j=1,SIZE(source_orientation, DIM = 2))				
	
				IF (istat.ne.0) CALL readerror (iunit,istat,filename)

			END DO cycleso

			CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'source_orientation read '
		END IF


	END IF

	!	Read the cdf of the source time from file

	IF (source%time%kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%source_time%filename),filestatus='OLD')		
			READ (iunit,'(A32)') chlog	!skip the intestation string
			IF (l_writelog) WRITE (99,*) chlog

cyclest:	DO i=1,cdf%source_time%nbins

			!the first column rapresents the x'value

				READ (iunit,*,IOSTAT=istat)	(source_time(i,j),j=1,SIZE(source_time, DIM = 2))				
	
				IF (istat.ne.0) CALL readerror (iunit,istat,filename)

			END DO cyclest

			CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'source_time read '
		END IF

	END IF

	!	Read the cdf of the source x position from file

	IF (source%x%kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%source_x%filename),filestatus='OLD')		
			READ (iunit,'(A32)') chlog	!skip the intestation string
			IF (l_writelog) WRITE (99,*) chlog

cyclesx:	DO i=1,cdf%source_x%nbins

			!the first column rapresents the x'value

				READ (iunit,*,IOSTAT=istat)	(source_x(i,j),j=1,SIZE(source_x, DIM = 2))				
	
				IF (istat.ne.0) CALL readerror (iunit,istat,filename)

			END DO cyclesx

			CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'source_x read '
		END IF

	END IF

	!	Read the cdf of the source y position from file

	IF (source%y%kind.EQ.'cdf') THEN
		CALL openfile (iunit,pathname,TRIM(cdf%source_y%filename),filestatus='OLD')		
			READ (iunit,'(A32)') chlog	!skip the intestation string
			IF (l_writelog) WRITE (99,*) chlog

cyclesy:	DO i=1,cdf%source_y%nbins

			!the first column rapresents the y'value

				READ (iunit,*,IOSTAT=istat)	(source_y(i,j),j=1,SIZE(source_y, DIM = 2))				
	
				IF (istat.ne.0) CALL readerror (iunit,istat,filename)

			END DO cyclesy

			CLOSE (iunit)

		IF (l_writelog) THEN
			WRITE (99,*) 'source_y read '
		END IF

	END IF


	WRITE (*,*) 'read cdfs OK'

	IF (l_writelog) THEN
		WRITE (99,*) 'all cdf read'
	END IF
	
END SUBROUTINE 	load_cdfs
