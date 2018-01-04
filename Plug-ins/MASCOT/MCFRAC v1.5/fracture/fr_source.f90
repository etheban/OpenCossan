!  Fr_source (l_source).f90
!
!  Subroutine Fr_source (l_source) 
!

SUBROUTINE fr_source1d 

	USE mod_global

	USE mod_dynamic_allocation

	USE mod_sampling

! Variables declaration

	IMPLICIT NONE
	INTEGER,SAVE:: itrialsource=0
	INTEGER:: iret,ib !unused variable
	REAL:: mean, std

	! body of the subroutine



!rt_part%old%time

	num_particles%in=num_particles%in+1
	itrialsource=itrialsource+1

!
!-------- Particle x-position ------------------------------
!

	if (itrialsource.eq.38) THEN
		WRITE (*,*)
	END IF

	SELECT CASE (TRIM(source%x%kind))
		CASE ('uni')
			rt_part%old%x=f_dist_uni_meanspan (source%x%par1,source%x%par2) 

		CASE ('uni_ss')
			rt_part%old%x=f_dist_uni_startstop (source%x%par1,source%x%par2) 

		CASE ('gau')
			mean = source%x%par1
			std = source%x%par2
			rt_part%old%x=f_dist_gauss (mean,std)

		CASE ('cdf')
			p_cdf=>source_x(:,2)
			p_bin=>source_x(:,1)
			CALL cdf_sampling(rt_part%old%x,iret,cdf%source_x%kind)	
			
		CASE ('pro')
			rt_part%old%x=f_dist_pro(source%x%par1,source%x%par2)

		CASE DEFAULT !It's a delta of kroneker
			rt_part%old%x=source%x%par1
	END SELECT

!	izone=rt_part%old%x/spece_step + 1

	CALL fr_zoning

!
!-------- Particle time ------------------------------
!

	SELECT CASE (TRIM(source%time%kind))
		CASE ('uni')
			rt_part%old%time=f_dist_uni_meanspan (source%time%par1,source%time%par2) 

		CASE ('uni_ss')
			rt_part%old%time=f_dist_uni_startstop (source%time%par1,source%time%par2) 

		CASE ('gau')
			mean = source%time%par1
			std = source%time%par2
			rt_part%old%time=f_dist_gauss (mean,std)
			
		CASE ('pro')
			rt_part%old%time=f_dist_pro(source%time%par1,source%time%par2)

		CASE ('cdf')
			p_cdf=>source_time(:,2)
			p_bin=>source_time(:,1)
			CALL cdf_sampling(rt_part%old%time,iret,cdf%source_time%kind)	
	
		CASE DEFAULT !It's a delta of kroneker
			rt_part%old%time=source%time%par1
	END SELECT

!
!-------- Particle kind ------------------------------
!
	rt_part%old%ikind=source%ikind

!
!-------- Particle source velocity ------------------------------
!

	SELECT CASE (TRIM(source%velocity%kind))		
		CASE ('uni')
			rt_part%old%fracture%velocity%value=f_dist_uni_meanspan (source%velocity%par1,source%velocity%par2) 
		CASE ('gau')
			rt_part%old%fracture%velocity%value=f_dist_gauss (source%velocity%par1,source%velocity%par2)
		CASE ('log') 
			rt_part%old%fracture%velocity%value=f_dist_logn (source%velocity%par1,source%velocity%par2)
		CASE ('pro')
			rt_part%old%fracture%velocity%value=f_dist_pro(source%velocity%par1,source%velocity%par2)	
		CASE ('cdf')
			p_cdf=>source_velocity(:,2)	!input for cdf_sampling (cdf)
			p_bin=>source_velocity(:,1) !input for cdf_sampling (bins)
			CALL cdf_sampling(rt_part%old%fracture%velocity%value,rt_part%old%fracture%velocity%bin,cdf%source_vel%kind)	
		CASE ('fix')
			rt_part%old%fracture%velocity%value=source%velocity%par1
		CASE DEFAULT !fixed value
			WRITE (*,*) source%velocity%kind,' not yet included'
			STOP
	END SELECT


!
!-------- Particle source orientation ---------------------------
!
	SELECT CASE (TRIM(source%orientation%kind))		
		CASE ('uni')
			rt_part%old%fracture%orientation%value=f_dist_uni_meanspan (source%orientation%par1,source%orientation%par2) 
		CASE ('gau')
			rt_part%old%fracture%orientation%value=f_dist_gauss (source%orientation%par1,source%orientation%par2)
		CASE ('log') 
			rt_part%old%fracture%orientation%value=f_dist_logn (source%orientation%par1,source%orientation%par2)
		CASE ('pro')
			rt_part%old%fracture%orientation%value=f_dist_pro(source%orientation%par1,source%orientation%par2)	
		CASE ('cdf')
			IF (l_corr_source) THEN
				p_cdf=>source_orientation(:,rt_part%old%fracture%velocity%bin+1)
				!CHECK CDF	
				IF (p_cdf(SIZE(p_cdf)).eq.0) THEN
					WRITE (*,*) 'Warning pdf orientation'
				END IF
			ELSE
				p_cdf=>source_orientation(:,2)
			END IF
			p_bin=>source_orientation(:,1)
			CALL cdf_sampling(rt_part%old%fracture%orientation%value,rt_part%old%fracture%orientation%bin,cdf%source_ori%kind)	
		CASE ('fix')
			rt_part%old%fracture%orientation%value=source%orientation%par1
		CASE DEFAULT !fixed value
			WRITE (*,*) source%orientation%kind,' not yet included'
			STOP
	END SELECT

	IF (itrialsource.EQ.source%ntrials) THEN ! process next source
		l_source=.FALSE.
	END IF

	! compute the corrent bin for the velocity and orientation if the correlation is activated
	IF (l_correlation) THEN
		!check the orientation
		p_bin => cdf_orientation(:,1,1,1)
		ib=1
		DO WHILE (rt_part%old%fracture%orientation%value.GT.p_bin(ib))
			ib=ib+1
			IF (ib.gt.SIZE(p_bin))THEN
				WRITE (*,*) 'Source orientation bin error'
				STOP 
			END IF
		END DO
		rt_part%old%fracture%orientation%bin=ib

		!check the velocity
		p_bin => cdf_velocity(:,1,1,1)
		ib=1
		DO WHILE (rt_part%old%fracture%velocity%value.GT.p_bin(ib))
			ib=ib+1
			IF (ib.gt.SIZE(p_bin))THEN
				WRITE (*,*) 'Source velocity bin error'
				STOP 
			END IF
		END DO
		rt_part%old%fracture%velocity%bin=ib

		! check the current bin

		p_cdf=>cdf_velocity(:,2,rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin)
		IF (p_cdf(SIZE(p_cdf)).eq.0) THEN
			IF (l_writelog) THEN
				WRITE (99,*) 'source: cdf velocity equal to zero'
			END IF
!			WRITE (*,*) 'WARNING:: cdf_vel=0 particle aborted'
			lost_particle=.TRUE.
			num_particles%aborted_source=num_particles%aborted_source+1
!			WRITE (*,*) rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin
		   	WRITE (98,*) 'Error sampling source'
			WRITE (98,*) 'rt_part%old%fracture%velocity%bin rt_part%old%fracture%orientation%bin'
			WRITE (98,*)  rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin
		END IF 
		
	END IF


	IF ((rt_part%old%x.lt.id_zones(izone)%W%border).OR.(rt_part%old%x.gt.id_zones(izone)%E%border)) THEN
		WRITE (*,*) 'position error: source'
		CALL fr_zoning
	END IF
	
	IF (l_writelog) THEN
		WRITE (99,*) 'fr_source1d,itrialsource:',itrialsource
		WRITE (98,*) 'itrialsource:',itrialsource
	END IF

	IF ((beta%output).AND.(.NOT.lost_particle)) THEN
		! Sample the new aperture	
		SELECT CASE (TRIM(fr_property%aperture%dist_kind))		
			CASE ('uni')
				rt_part%old%fracture%aperture%value=f_dist_uni_meanspan (fr_property%aperture%mean,fr_property%aperture%span) 
			CASE ('gau')
				rt_part%old%fracture%aperture%value=f_dist_gauss (fr_property%aperture%mean,fr_property%aperture%std)
			CASE ('log') 
				rt_part%old%fracture%aperture%value=f_dist_logn (fr_property%aperture%mean,fr_property%aperture%std)
			CASE ('cdf')

				IF (l_correlation) THEN
					p_cdf=>cdf_aperture(:,2,rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin)
					p_bin=>cdf_aperture(:,1,rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin)
				ELSE
					p_cdf=>cdf_aperture(:,2,1,1)
					p_bin=>cdf_aperture(:,1,1,1)
				END IF

				IF (p_cdf(SIZE(p_cdf)).eq.0) THEN
!					WRITE (*,*) 'aperture cdf=0 in the source file' 
!					WRITE (*,*)  rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin
		   			WRITE (98,*) 'Error sampling aperture'
					WRITE (98,*) 'rt_part%old%fracture%velocity%bin rt_part%old%fracture%orientation%bin'
					WRITE (98,*)  rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin
				ELSE

					CALL cdf_sampling(rt_part%old%fracture%aperture%value,rt_part%old%fracture%aperture%bin,cdf%aperture%kind)
		
				END IF
					
			CASE ('fix')
				rt_part%new%fracture%aperture%value=fr_property%aperture%mean
			CASE DEFAULT !fixed value
				WRITE (*,*) fr_property%aperture%dist_kind,' not yet included'
				STOP
		END SELECT
	END IF

END SUBROUTINE fr_source1d

! Sample source for 2d model

SUBROUTINE fr_source2d 

	USE mod_global

	USE mod_dynamic_allocation

	USE mod_sampling



! Variables declaration

	IMPLICIT NONE
	REAL:: mean, std
	INTEGER:: iret

	! body of the subroutine


!
!-------- Particle y-position ------------------------------
!

	SELECT CASE (TRIM(source%y%kind))
		CASE ('uni')
			rt_part%old%y=f_dist_uni_meanspan (source%y%par1,source%y%par2) 

		CASE ('uni_ss')
			rt_part%old%y=f_dist_uni_startstop (source%y%par1,source%y%par2) 

		CASE ('gau')
			mean = source%y%par1
			std = source%y%par2
			rt_part%old%y=f_dist_gauss (mean,std)
		CASE ('pro')
			rt_part%old%y=f_dist_pro(source%y%par1,source%y%par2)	

		CASE ('cdf')

			p_cdf=>source_y(:,2)
			p_bin=>source_y(:,1)

			CALL cdf_sampling(rt_part%old%y,iret,cdf%source_y%kind)

		CASE ('fix')
			rt_part%old%y=source%y%par1

		CASE DEFAULT !It's a delta of kroneker
			rt_part%old%y=source%y%par1
	END SELECT

	!izone=rt_part%old%y/spece_step + 1

	CALL fr_source1d

END SUBROUTINE fr_source2d
