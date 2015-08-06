!v.01  
!  redistribution_kernel.f90
!
!  Subroutine redistribution_kernel
!	
!
SUBROUTINE redistribution_kernel

!	use modules

	USE mod_global

	USE mod_dynamic_allocation

	USE mod_sampling
	
  ! Variables

	IMPLICIT NONE

! Debug
!	IF ((rt_part%old%fracture%velocity%bin.eq.77).AND.(rt_part%old%fracture%orientation%bin.eq.12).AND.(num_particles%in.eq.371044) ) THEN
!		WRITE (*,*)
!	END IF
! end debug

	! Sample first the new velocity (from the marginal distribution if the correlations are activated)
	! and then the new orientation

	! Sample the new velocity	

	SELECT CASE (TRIM(fr_property%velocity%dist_kind))		
		CASE ('uni')
			rt_part%new%fracture%velocity%value=f_dist_uni_meanspan (fr_property%velocity%mean,fr_property%velocity%span) 
		CASE ('gau')
			rt_part%new%fracture%velocity%value=f_dist_gauss (fr_property%velocity%mean,fr_property%velocity%std)
		CASE ('log') 
			rt_part%new%fracture%velocity%value=f_dist_logn (fr_property%velocity%mean,fr_property%velocity%std)
		CASE ('cdf')

			IF (l_correlation) THEN
				p_cdf=>cdf_velocity(:,2,rt_part%old%fracture%velocity%bin, &
&                                      rt_part%old%fracture%orientation%bin)
				p_bin=>cdf_velocity(:,1,rt_part%old%fracture%velocity%bin, &
&                                      rt_part%old%fracture%orientation%bin)
			ELSE
				p_cdf=>cdf_velocity(:,2,1,1)
				p_bin=>cdf_velocity(:,1,1,1)
			END IF

			CALL cdf_sampling(rt_part%new%fracture%velocity%value, &
&                                         rt_part%new%fracture%velocity%bin,cdf%velocity%kind)


			IF (p_cdf(SIZE(p_cdf)).eq.0) THEN
				IF (l_writelog) THEN
					WRITE (99,*) 'cdf velocity equal to zero'
				END IF
				WRITE (*,*) 'WARNING:: cdf vel=0'
				STOP
			END IF
		
		CASE ('fix')
			rt_part%new%fracture%velocity%value=fr_property%velocity%mean
		CASE DEFAULT !fixed value
			WRITE (*,*) fr_property%velocity%dist_kind,' not yet included'
			STOP
	END SELECT

	!Debug
	IF (rt_part%new%fracture%velocity%value.le.0) THEN
		write (*,*) 'Sampling velocity error!'
	END IF
	!end debug

	IF (l_writelog) THEN
		WRITE (98,*) 'rt_part%new%fracture%velocity%bin'
		WRITE (98,*)  rt_part%new%fracture%velocity%bin
	END IF


	! Sample the new orientation
	
	SELECT CASE (TRIM(fr_property%orientation%dist_kind))		
		CASE ('uni')
			rt_part%new%fracture%orientation%value=f_dist_uni_meanspan (fr_property%orientation%mean,fr_property%orientation%span) 
		CASE ('gau')
			rt_part%new%fracture%orientation%value=f_dist_gauss (fr_property%orientation%mean,fr_property%orientation%std)
		CASE ('log') 
			rt_part%new%fracture%orientation%value=f_dist_logn (fr_property%orientation%mean,fr_property%orientation%std)
		CASE ('cdf')

			IF (l_correlation) THEN
                            p_cdf=>cdf_orientation(:,rt_part%new%fracture%velocity%bin+1, &
&                                  rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin)
                            p_bin=>cdf_orientation(:,1,rt_part%old%fracture%velocity%bin, &
&                                  rt_part%old%fracture%orientation%bin)
			ELSE
				p_cdf=>cdf_orientation(:,2,1,1)
				p_bin=>cdf_orientation(:,1,1,1)
			END IF

			CALL cdf_sampling(rt_part%new%fracture%orientation%value,rt_part%new%fracture%orientation%bin,cdf%orientation%kind)
		
		CASE ('fix')
			rt_part%new%fracture%orientation%value=fr_property%orientation%mean				
		CASE ('wil')
			rt_part%new%fracture%orientation%value=f_dist_uni_meanspan (fr_property%orientation%mean,fr_property%orientation%span) 
		CASE DEFAULT !fixed value
			WRITE (*,*) fr_property%orientation%dist_kind,' not yet included'
			STOP
	END SELECT

	IF (l_writelog) THEN
		WRITE (98,*) 'rt_part%new%fracture%orientation%bin'
		WRITE (98,*)  rt_part%new%fracture%orientation%bin
	END IF


	!Check the new particle state

	IF (l_correlation) THEN

		p_cdf=>cdf_velocity(:,2,rt_part%new%fracture%velocity%bin,rt_part%new%fracture%orientation%bin)
	
		IF (p_cdf(SIZE(p_cdf)).eq.0) THEN
			IF (l_writelog) THEN
				WRITE (99,*) 'redist.func.: cdf=0 This state has NOT correlation'
			END IF
!			WRITE (*,*) 'WARNING:: cdf=0 particle aborted in redistribution function'
			lost_particle=.TRUE.
			num_particles%aborted_rf=num_particles%aborted_rf+1
!			WRITE (*,*) rt_part%new%fracture%velocity%bin,rt_part%new%fracture%orientation%bin
		   	WRITE (98,*) 'Error redistribution function'
			WRITE (98,*) 'rt_part%old%fracture%velocity%bin rt_part%old%fracture%orientation%bin'
			WRITE (98,*)  rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin
			WRITE (98,*) 'rt_part%new%fracture%velocity%bin rt_part%new%fracture%orientation%bin'
			WRITE (98,*)  rt_part%new%fracture%velocity%bin,rt_part%new%fracture%orientation%bin
		END IF 

	END IF


	IF ((beta%output).AND.(.NOT.lost_particle)) THEN
		! Sample the new aperture	
		SELECT CASE (TRIM(fr_property%aperture%dist_kind))		
			CASE ('uni')
				rt_part%new%fracture%aperture%value=f_dist_uni_meanspan &
&                                   (fr_property%aperture%mean,fr_property%aperture%span) 
			CASE ('gau')
				rt_part%new%fracture%aperture%value=f_dist_gauss &
&                                   (fr_property%aperture%mean,fr_property%aperture%std)
			CASE ('log') 
				rt_part%new%fracture%aperture%value=f_dist_logn  &
&                                   (fr_property%aperture%mean,fr_property%aperture%std)
			CASE ('cdf')

				IF (l_correlation) THEN
					p_cdf=>cdf_aperture(:,2,rt_part%new%fracture%velocity%bin, &
&                                              rt_part%new%fracture%orientation%bin)
					p_bin=>cdf_aperture(:,1,rt_part%new%fracture%velocity%bin, &
&                                              rt_part%new%fracture%orientation%bin)
				ELSE
					p_cdf=>cdf_aperture(:,2,1,1)
					p_bin=>cdf_aperture(:,1,1,1)
				END IF

				IF (p_cdf(SIZE(p_cdf)).eq.0) THEN
!					WRITE (*,*) 'aperture cdf=0' 
!					WRITE (*,*)  rt_part%new%fracture%velocity%bin,rt_part%new%fracture%orientation%bin
		   			WRITE (98,*) 'Error sampling aperture'
					WRITE (98,*) 'rt_part%new%fracture%velocity%bin rt_part%new%fracture%orientation%bin'
					WRITE (98,*)  rt_part%new%fracture%velocity%bin,rt_part%new%fracture%orientation%bin
					
					rt_part%new%fracture%aperture%value=rt_part%old%fracture%aperture%value
					beta%part_err=beta%part_err+1
				ELSE

					CALL cdf_sampling(rt_part%new%fracture%aperture%value, &
&                                            rt_part%new%fracture%aperture%bin,cdf%aperture%kind)
		
				END IF

			CASE ('fix')
				rt_part%new%fracture%aperture%value=fr_property%aperture%mean
			CASE DEFAULT !fixed value
				WRITE (*,*) fr_property%aperture%dist_kind,' not yet included'
				STOP
		END SELECT
	END IF



	
	IF (l_writelog) THEN
		WRITE (99,*) 'redistribution_kernel'
	END IF
	
END SUBROUTINE redistribution_kernel 
