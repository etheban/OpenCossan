!v.01  
!  scattering_kernel.f90
!
!  Subroutine scattering_kernel
!	
!
SUBROUTINE scattering_kernel

!	use modules

	USE mod_global

	USE mod_dynamic_allocation

	USE mod_sampling
	
  ! Variables

	IMPLICIT NONE

	SELECT CASE (TRIM(fr_property%length%dist_kind))		
		CASE ('uni')
			rt_part%new%fracture%length%value=f_dist_uni_meanspan (fr_property%length%mean,fr_property%length%span) 
		CASE ('gau')
			rt_part%new%fracture%length%value=f_dist_gauss (fr_property%length%mean,fr_property%length%std)
		CASE ('log') 
			rt_part%new%fracture%length%value=f_dist_logn (fr_property%length%mean,fr_property%length%std)
		CASE ('cdf')

			IF (l_correlation) THEN
				p_cdf=>cdf_length(:,2,rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin)
				p_bin=>cdf_length(:,1,rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin)
			ELSE
				p_cdf=>cdf_length(:,2,1,1)
				p_bin=>cdf_length(:,1,1,1)
			END IF

			CALL cdf_sampling(rt_part%new%fracture%length%value,rt_part%new%fracture%length%bin,cdf%length%kind)

		CASE ('fix')
			rt_part%new%fracture%length%value=fr_property%length%mean

		CASE ('lmv') !the fracture length is a function of the current velocity and orientation
			rt_part%new%fracture%length%value= &
&			cdf_length(1,1,rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin)
		CASE DEFAULT !fixed value
			WRITE (*,*) fr_property%length%dist_kind,' not yet included'
			STOP
	END SELECT
	
	IF (l_writelog) THEN
		WRITE (99,*) 'scattering_kernel '
	END IF

END SUBROUTINE scattering_kernel 
