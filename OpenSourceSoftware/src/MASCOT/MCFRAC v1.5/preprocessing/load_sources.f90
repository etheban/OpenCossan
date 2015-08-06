! Read source file 
!

SUBROUTINE load_sources 


! Variables Declariation 

	USE mod_dynamic_allocation

	USE mod_global

	IMPLICIT NONE

	CHARACTER*16::	keyword

!	CHARACTER*16:: filename,pathname
!
!	filename='sources.txt'	 !default filename of the souces matrix 
!	pathname='input/'	 !Default path of the input file
!
!	iunit=22
!
!	CALL openfile (iunit,pathname,filename,filestatus='OLD')
!
!	Body of the subroutine

readinput:	DO

			READ (iunit,'(A16)') keyword 

			SELECT CASE (TRIM(keyword))

				CASE ('VELOCITY') !velocity source

					READ(iunit,*) source%velocity%kind
					
					SELECT CASE (source%velocity%kind)

						CASE ('cdf')
							READ(iunit,*) cdf%source_vel%filename		
							READ(iunit,*) cdf%source_vel%kind
							READ(iunit,*) cdf%source_vel%nbins
!							READ(iunit,*) cdf%source_vel%ndims
						CASE ('uni')
							READ (iunit,*) source%velocity%par1
							READ (iunit,*) source%velocity%par2
						CASE ('pro')
							READ (iunit,*) source%velocity%par1
							READ (iunit,*) source%velocity%par2
						CASE ('fix')
							READ (iunit,*) source%velocity%par1
						CASE ('gau')
							READ (iunit,*) source%velocity%par1
							READ (iunit,*) source%velocity%par2
						CASE ('lon')
							READ (iunit,*) source%velocity%par1
							READ (iunit,*) source%velocity%par2
						CASE DEFAULT
							WRITE (*,*) ' Error: velocity distribution not-recognoised'
							STOP
					END SELECT 

				CASE ('ORIENTATION') !scattering kernel
	
					READ(iunit,*) source%orientation%kind
					
					SELECT CASE (source%orientation%kind)

						CASE ('cdf')
							READ(iunit,*) cdf%source_ori%filename		
							READ(iunit,*) cdf%source_ori%kind
							READ(iunit,*) cdf%source_ori%nbins
!							READ(iunit,*) cdf%source_ori%ndims
						CASE ('uni')
							READ (iunit,*) source%orientation%par1
							READ (iunit,*) source%orientation%par2
						CASE ('fix')
							READ (iunit,*) source%orientation%par1
						CASE ('pro')
							READ (iunit,*) source%orientation%par1
							READ (iunit,*) source%orientation%par2
						CASE ('gau')
							READ (iunit,*) source%orientation%par1
							READ (iunit,*) source%orientation%par2
						CASE ('lon')
							READ (iunit,*) source%orientation%par1
							READ (iunit,*) source%orientation%par2
						CASE DEFAULT
							WRITE (*,*) ' Error: orientation distribution not-recognoised'
							STOP
					END SELECT 

			CASE ('X') !scattering kernel
	
					READ(iunit,*) source%x%kind
					
					SELECT CASE (source%x%kind)

						CASE ('cdf')
							READ(iunit,*) cdf%source_x%filename		
							READ(iunit,*) cdf%source_x%kind
							READ(iunit,*) cdf%source_x%nbins
!							READ(iunit,*) cdf%source_x%ndims
						CASE ('uni')
							READ (iunit,*) source%x%par1
							READ (iunit,*) source%x%par2
						CASE ('fix')
							READ (iunit,*) source%x%par1
						CASE ('pro')
							READ (iunit,*) source%x%par1
							READ (iunit,*) source%x%par2
						CASE ('gau')
							READ (iunit,*) source%x%par1
							READ (iunit,*) source%x%par2
						CASE ('lon')
							READ (iunit,*) source%x%par1
							READ (iunit,*) source%x%par2
						CASE DEFAULT
							WRITE (*,*) ' Error: x distribution not-recognoised'
							STOP
					END SELECT 

				CASE ('Y') !scattering kernel
	
					READ(iunit,*) source%y%kind
					
					SELECT CASE (source%y%kind)

						CASE ('cdf')
							READ(iunit,*) cdf%source_y%filename		
							READ(iunit,*) cdf%source_y%kind
							READ(iunit,*) cdf%source_y%nbins
!							READ(iunit,*) cdf%source_y%ndims
						CASE ('uni')
							READ (iunit,*) source%y%par1
							READ (iunit,*) source%y%par2
						CASE ('fix')
							READ (iunit,*) source%y%par1
						CASE ('pro')
							READ (iunit,*) source%y%par1
							READ (iunit,*) source%y%par2
						CASE ('gau')
							READ (iunit,*) source%y%par1
							READ (iunit,*) source%y%par2
						CASE ('lon')
							READ (iunit,*) source%y%par1
							READ (iunit,*) source%y%par2
						CASE DEFAULT
							WRITE (*,*) ' Error: y distribution not-recognoised'
							STOP
					END SELECT 

				CASE ('TIME') !scattering kernel
	
					READ(iunit,*) source%time%kind
					
					SELECT CASE (source%time%kind)

						CASE ('cdf')
							READ(iunit,*) cdf%source_time%filename		
							READ(iunit,*) cdf%source_time%kind
							READ(iunit,*) cdf%source_time%nbins
!							READ(iunit,*) cdf%source_time%ndims
						CASE ('uni')
							READ (iunit,*) source%time%par1
							READ (iunit,*) source%time%par2
						CASE ('fix')
							READ (iunit,*) source%time%par1
						CASE ('pro')
							READ (iunit,*) source%time%par1
							READ (iunit,*) source%time%par2
						CASE ('gau')
							READ (iunit,*) source%time%par1
							READ (iunit,*) source%time%par2
						CASE ('lon')
							READ (iunit,*) source%time%par1
							READ (iunit,*) source%time%par2
						CASE DEFAULT
							WRITE (*,*) ' Error: time distribution not-recognoised'
							STOP
					END SELECT 

				CASE ('KIND')

					READ (iunit,*) source%ikind

				CASE ('TRIALS')

					READ (iunit,*) source%ntrials

				CASE ('CORRELATED')

					l_corr_source=.TRUE.
					WRITE (*,*) 'Soruce velocities and orientation correlated'
	
				CASE ('END SOURCE')

						EXIT readinput

			END SELECT
				
		END DO readinput


!	CLOSE (unit=iunit)


	IF (l_writelog) THEN
		WRITE (99,*) 'Source read'
		WRITE (99,*) source
	END IF


END SUBROUTINE load_sources
