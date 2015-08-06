!  Author: Edoardo Patelli
!  Politecnico di Milano 
!
!  File Input_fracture_propeties.f90
!
! Load the parameters of the fractures


SUBROUTINE input_fracture_propeties

!	use modules

	USE mod_global

! Variables Declariation 

	IMPLICIT NONE

	CHARACTER*16::	keyword


!	Body of the subroutine

	CALL openfile (iunit,pathname='input/',filename='fr_input.txt',filestatus='OLD')	

!	write (17,*) 'SUBROUTINE input_fracture_propeties: OK'


readinput:	DO

			READ (iunit,'(A16)') keyword 

			SELECT CASE (TRIM(keyword))

				CASE ('SOURCE')  !Read source kind

					IF (l_writelog) WRITE (99,*) keyword
						
					 CALL load_sources  !Load the source 

				CASE ('VELOCITY') !velocity kernel kind

					IF (l_writelog) WRITE (99,*) keyword

					READ(iunit,*) fr_property%velocity%dist_kind
					
					SELECT CASE (fr_property%velocity%dist_kind)

						CASE ('cdf')
							READ(iunit,*) cdf%velocity%filename		
							READ(iunit,*) cdf%velocity%kind
							READ(iunit,*) cdf%velocity%nbins
!							READ(iunit,*) cdf%velocity%ndims
						CASE ('uni')
							READ(iunit,*) fr_property%velocity%mean
							READ(iunit,*) fr_property%velocity%span
						CASE ('fix')
							READ(iunit,*) fr_property%velocity%mean
						CASE ('gau')
							READ(iunit,*) fr_property%velocity%mean
							READ(iunit,*) fr_property%velocity%std
						CASE ('lon')
							READ(iunit,*) fr_property%velocity%mean
							READ(iunit,*) fr_property%velocity%std
						CASE DEFAULT
							WRITE (*,*) ' Error: velocity distribution not-recognoised'
							STOP
					END SELECT 

				CASE ('ORIENTATION') !scattering kernel

					IF (l_writelog) WRITE (99,*) keyword
	
					READ(iunit,*) fr_property%orientation%dist_kind
				
					SELECT CASE (fr_property%orientation%dist_kind)

						CASE ('cdf')
							READ(iunit,*) cdf%orientation%filename		
							READ(iunit,*) cdf%orientation%kind
							READ(iunit,*) cdf%orientation%nbins
!							READ(iunit,*) cdf%orientation%ndims
						CASE ('uni')
							READ(iunit,*) fr_property%orientation%mean
							READ(iunit,*) fr_property%orientation%span
						CASE ('fix')
							READ(iunit,*) fr_property%orientation%mean
						CASE ('gau')
							READ(iunit,*) fr_property%orientation%mean
							READ(iunit,*) fr_property%orientation%std
						CASE ('lon')
							READ(iunit,*) fr_property%orientation%mean
							READ(iunit,*) fr_property%orientation%std
						CASE ('wil')
							READ(iunit,*) fr_property%orientation%mean
							READ(iunit,*) fr_property%orientation%span
						CASE DEFAULT
							WRITE (*,*) ' Error: orientation distribution not-recognoised'
							STOP
					END SELECT 

				CASE ('LENGTH') !Redistribution kernel

					IF (l_writelog) WRITE (99,*) keyword
	
						READ(iunit,*) fr_property%length%dist_kind

					SELECT CASE (fr_property%length%dist_kind)

						CASE ('cdf')
							READ(iunit,*) cdf%length%filename		
							READ(iunit,*) cdf%length%kind
							READ(iunit,*) cdf%length%nbins
!							READ(iunit,*) cdf%length%ndims
						CASE ('uni')
							READ(iunit,*) fr_property%length%mean
							READ(iunit,*) fr_property%length%span
						CASE ('gau')
							READ(iunit,*) fr_property%length%mean
							READ(iunit,*) fr_property%length%std
						CASE ('lon')
							READ(iunit,*) fr_property%length%mean
							READ(iunit,*) fr_property%length%std
						CASE ('fix')
							READ(iunit,*) fr_property%length%mean
						CASE ('lmv')
							READ(iunit,*) cdf%length%filename
							cdf%length%kind='fix'
!							cdf%length%nbins=1
							
						CASE DEFAULT
							WRITE (*,*) ' Error: length distribution not-recognoised'
							STOP

					END SELECT

				CASE ('APERTURE') !aperture kernel kind

					IF (l_writelog) WRITE (99,*) keyword

					READ(iunit,*) fr_property%aperture%dist_kind
					
					SELECT CASE (fr_property%aperture%dist_kind)

						CASE ('cdf')
							READ(iunit,*) cdf%aperture%filename		
							READ(iunit,*) cdf%aperture%kind
							READ(iunit,*) cdf%aperture%nbins
!							READ(iunit,*) cdf%aperture%ndims
						CASE ('uni')
							READ(iunit,*) fr_property%aperture%mean
							READ(iunit,*) fr_property%aperture%span
						CASE ('fix')
							READ(iunit,*) fr_property%aperture%mean
						CASE ('gau')
							READ(iunit,*) fr_property%aperture%mean
							READ(iunit,*) fr_property%aperture%std
						CASE ('lon')
							READ(iunit,*) fr_property%aperture%mean
							READ(iunit,*) fr_property%aperture%std
						CASE DEFAULT
							WRITE (*,*) ' Error: aperture distribution not-recognoised'
							STOP
					END SELECT 

				CASE ('OUTPUTFILENAME')

				IF (l_writelog) WRITE (99,*) keyword
	
						READ(iunit,*) output_filename1
						READ(iunit,*) l_writelog
						READ(iunit,*) l_variance

				CASE ('BETA')
					IF (l_writelog) WRITE (99,*) keyword
					beta%output=.TRUE.			!flag for the output file with beta
					READ(iunit,*) beta%filename !filename of the beta file
 					READ(iunit,*) beta%x        !x-position of the control panel for beta
					READ(iunit,*) beta%y        !y-position of the control panel for beta
					
				CASE ('TAU')
					IF (l_writelog) WRITE (99,*) keyword
					tau%output=.TRUE.			    !flag for the output file with beta
					READ(iunit,*) tau%filename !filename of the beta file
 					READ(iunit,*) tau%x        !x-position of the control panel for beta
					READ(iunit,*) tau%y        !y-position of the control panel for beta
						
				CASE ('MC')
					IF (l_writelog) WRITE (99,*) keyword
					READ(iunit,*)	iseed		!Seed for the random variables	
					READ(iunit,*)	time_sim_start
					READ(iunit,*)	time_sim_end						
					READ(iunit,*) 	n_timech
					READ(iunit,*)	ntot_reactions		!Total number of reaction rates	
					READ(iunit,*)	ntot_movements		!Total number of movement rates	
!					READ(iunit,*)	ntot_nonlinearity	!Total number of nonlinear rates
					READ(iunit,*)	n_zone	!total number of zones
					READ(iunit,*)	n_kind	!total number of particle kinds
					READ(iunit,*)	l_correlation       !enable correlation in fracture properties

				CASE ('DOMAIN')
					IF (l_writelog) WRITE (99,*) keyword
					READ(iunit,*)	n_spacedim	! space dimension of the domain (Geometry)
					READ(iunit,*)	spece_step  ! cell dimension
				   READ(iunit,*)	l_border  	! Automatic borders (T) or read from file (F)
				   
!				   WRITE(*,*) ' read from input file: ', n_spacedim,spece_step,l_border
	
				CASE ('END')
					IF (l_writelog) WRITE (99,*) keyword
					WRITE (*,*) 'read input file: OK'
					EXIT readinput

				CASE ('DECAY')
					IF (l_writelog) WRITE (99,*) keyword
					READ(iunit,*)	lambda	!decay rate
				CASE ('')

			END SELECT
				
		END DO readinput

	CLOSE (unit=iunit)

	! compute some parameters of interest

	width_timech=time_sim_end/n_timech
	width_timech2=width_timech**2
	
END SUBROUTINE input_fracture_propeties
