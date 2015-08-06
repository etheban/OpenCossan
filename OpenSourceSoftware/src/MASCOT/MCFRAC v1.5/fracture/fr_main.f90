!v.1.0  
!  Fr_main.f90
!
!  Subroutine Fr_main
!	
!
SUBROUTINE Fr_main 

!	use modules

   USE mod_global

   USE mod_dynamic_allocation

   USE mod_sampling

  ! Variables

   IMPLICIT NONE

   REAL:: travel_distance
   INTEGER:: i

! set parameter

! compute useful parameter to speed-up the code

   DO i=1,n_timech
      t_right_count(i)=width_timech*FLOAT(i)
   END DO
 
   num_particles%in=0
   num_particles%dead=0
   num_particles%aborted_source=0
   num_particles%aborted_rf=0
   num_particles%E=0
   num_particles%W=0
   num_particles%N=0
   num_particles%S=0
   num_particles%U=0
   num_particles%D=0
   
   l_source=.true.

   IF (beta%output) THEN
     CALL openfile (iunit=9,pathname='output/',filename=beta%filename,filestatus='NEW')
     beta%part_err=0
   END IF
   IF (tau%output) THEN
     CALL openfile (iunit=10,pathname='output/',filename=tau%filename,filestatus='NEW')
     tau%part_err=0
   END IF

!************************************************************************
!						Start of Monte Carlo simulation					*
!************************************************************************


   SELECT CASE (n_spacedim)
      CASE (1)

cycle_particle1d: DO 
   
             ! reset variable
				 lost_particle=.FALSE.
                 rt_part%old%time=0
                 rt_part%old%x=0
				 rt_part%old%y=0
				 !reset variables for beta
				 beta%bfp=.FALSE.
				 beta%value=0
				 
				 !reset variables for tau
				 tau%bfp=.FALSE.
				 tau%value=0
				 
				!+------------------------!
				! SOURCE of Particles     !      
				!+------------------------!
     
				IF (l_source) THEN !Sample zone and time of the source 
					CALL Fr_source1d 
					IF (lost_particle) CYCLE cycle_particle1d
				ELSE
					EXIT cycle_particle1d !All particles have been processed
				END IF
     
	            I_chan_old=rt_part%old%time/width_timech+1
	            
	           IF (l_writelog) THEN
					WRITE (99,*) 'Source sampled -> start time cycle'
		        	WRITE (98,*) 'Source sampled -> start time cycle'
					WRITE (98,*) 'rt_part%old%fracture%velocity%bin rt_part%old%fracture%orientation%bin'
					WRITE (98,*)  rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin
			   END IF

    
cycle_time1d: DO 

              ! compute the scattering kernel (scattering cross section)

                CALL scattering_kernel
 
                ! compute the collision kernel (total cross section)

                CALL collision_kernel
        		
				! Sample distance of collision

                !travel_distance=f_dist_exp(1/sigma_tot)

                !pass the mean free path 

                travel_distance=f_dist_exp_inv(sigma_tot_inv)

                mu=COS(rt_part%old%fracture%orientation%value) 	
				nu=SIN(rt_part%new%fracture%orientation%value)				          
                !update runtime values 
				 rt_part%new%time=rt_part%old%time+ &
					  & (travel_distance/rt_part%old%fracture%velocity%value)
				!x-space		
                 rt_part%new%x=rt_part%old%x+ &
					  & travel_distance*mu
		    	!y-space		
                 rt_part%new%y=rt_part%old%y+ &
					  & travel_distance*nu

 				! prevent fault counter updating
				IF (rt_part%new%time.EQ.rt_part%old%time) THEN
					IF (l_writelog) WRITE(99,*) 'WARNING: cycle time1d:time_new=time_old'
					CYCLE cycle_time1d
				END IF

				
				CALL fr_update_counter1d	! update counters

				IF (beta%output) THEN
				 IF(.NOT.beta%bfp) CALL fr_beta1d	! update beta counter
				END IF
				IF (tau%output) THEN
				 	IF(.NOT.tau%bfp) CALL fr_tau1d	! update tau counter
				END IF

				! Write log file
				IF (l_writelog) THEN
					WRITE (99,*) 'counter updated'
				END IF
      
				! select kind of transition (use this subroutine in general case)
				!CALL Fr_sample_transition
				
				!in this case the particles doas not change it's status
				rt_part%new%ikind=rt_part%old%ikind


				IF (rt_part%new%ikind.eq.ipartlost) THEN
					num_particles%dead=num_particles%dead+1
					lost_particle=.TRUE. !the particle is no more followed
				END IF

				IF (lost_particle) THEN
					CYCLE cycle_particle1d
				END IF

				!+------------------------!
				! Redistribution kernel   !      
				!+------------------------!

             CALL redistribution_kernel
             
             IF (l_writelog) THEN
					WRITE (99,*) 'rt_part%new%fracture'
					WRITE (99,*) rt_part%new%fracture
					WRITE (99,*) 'rt_part%new%x rt_part%new%time'
					WRITE (99,*) rt_part%new%x,rt_part%new%time
			   END IF

				!preparing for the next sample
				rt_part%old=rt_part%new


				IF ((rt_part%old%x.lt.id_zones(izone)%W%border).OR.(rt_part%old%x.gt.id_zones(izone)%E%border)) THEN
					!WRITE (*,*) 'position error'
					loginfo%particle_lost=loginfo%particle_lost+1
					CALL fr_zoning
					IF (lost_particle) CYCLE cycle_particle1d
				END IF

				CYCLE cycle_time1d  !sample next iteration
    
			END DO cycle_time1d
 
			IF (l_writelog) THEN
				WRITE (99,*) 'cycle time done'
				WRITE (99,*) num_particles 
			END IF

		END DO cycle_particle1d

	  CASE (2)

  cycle_particle2d: DO 
   
                 ! reset variables 
                 lost_particle=.FALSE.
                 rt_part%old%time=0
                 rt_part%old%x=0
                 rt_part%old%y=0
				 
				 !reset variables for beta
				 beta%bfp=.FALSE.
				 beta%value=0
				 
				 !reset variables for tau
				 tau%bfp=.FALSE.
				 tau%value=0
				 
				!+------------------------!
				! SOURCE of Particles     !      
				!+------------------------!
     
                   IF (l_source) THEN !Sample zone and time of the source 
                      CALL fr_source2d 
                   ELSE
                      EXIT cycle_particle2d !All particles have been processed
                   END IF
     
                   I_chan_old=rt_part%old%time/width_timech+1
     
                   ! reset space variable

					IF (l_writelog) THEN
						WRITE (99,*) 'Source sampled -> start time cycle'
				    	WRITE (98,*) 'Source sampled -> start time cycle'
						WRITE (98,*) 'rt_part%old%fracture%velocity%bin rt_part%old%fracture%orientation%bin'
						WRITE (98,*)  rt_part%old%fracture%velocity%bin,rt_part%old%fracture%orientation%bin
						WRITE (98,*) 'rt_part%old%fracture%aperture%bin'
						WRITE (98,*)  rt_part%old%fracture%aperture%bin

					END IF

       cycle_time2d: DO 

                     ! compute the scattering kernel (scattering cross section)

                     CALL scattering_kernel

                     ! compute the collision kernel (total cross section)

                     CALL collision_kernel

                     !pass the mean free path 
                     travel_distance=f_dist_exp_inv(sigma_tot)

					 mu=COS(rt_part%new%fracture%orientation%value)
					 nu=SIN(rt_part%new%fracture%orientation%value)
        
                     ! Sample distance of collision

                     !travel_distance=f_dist_exp(1/sigma_tot)

		             ! Debub only
                     IF (travel_distance.le.0) then
        		        WRITE (*,*) 'ERROR!!!!'
        				write (*,*) 'sampled travel distance =',travel_distance
        				write (*,*) 'sigma_tot=',sigma_tot
					 END IF
					! Debub only
    
                    !update runtime values
                    rt_part%new%time=rt_part%old%time+ &
					& (travel_distance/rt_part%new%fracture%velocity%value)
                    !x-space		
                    rt_part%new%x=rt_part%old%x+ &
					& travel_distance*mu*rt_part%new%fracture%velocity%value
                    !y-space		
                    rt_part%new%y=rt_part%old%y+ &
					travel_distance*nu*rt_part%new%fracture%velocity%value

                   ! compute probability COLLISION density (psi)
  
					CALL fr_update_counter2d

					IF (beta%output) THEN
						IF(.NOT.beta%bfp) CALL fr_beta2d	! update beta counter
					END IF
					IF (tau%output) THEN
				 		IF(.NOT.tau%bfp) CALL fr_tau2d	! update tau counter
					END IF
					
                   ! start log file
                   IF (l_writelog) THEN
                      WRITE (99,*) 'sample trajectories'
                      WRITE (99,*) rt_part%old,rt_part%new
                   END IF

                  ! select kind of transition
                  ! CALL Fr_sample_transition

                  IF (rt_part%new%ikind.eq.ipartlost) THEN
                     num_particles%dead=num_particles%dead+1
                     lost_particle=.TRUE. !the particle is no more followed
                  END IF

                  IF (lost_particle) THEN
					 CYCLE cycle_particle2d
				  END IF             
				  
		          !Sample free fly direction (orientation)
                     CALL redistribution_kernel


                  !preparing for the next sample
				  rt_part%old=rt_part%new

				! include lost_particle (as done in 1d simulation)


                  CYCLE cycle_time2d  !sample next iteration
    
		  	END DO cycle_time2d
 
			IF (l_writelog) THEN
				WRITE (99,*) 'cycle time done'
				WRITE (99,*) num_particles 
			END IF

		END DO cycle_particle2d

	  CASE (3)
		
		WRITE (*,*) '3d model han not been implemented'
		WRITE (*,*) 'Have you free time to do this?'
		STOP

   END SELECT

   ! normalize counter
   particle_counts=particle_counts/width_timech
   
   WRITE (*,*) 'Monte Carlo simulation done!'
   WRITE (*,*)

   WRITE (*,*) 'Particle aborted source = ',num_particles%aborted_source
   WRITE (*,*) 'Particle aborted red.fu = ',num_particles%aborted_rf
   WRITE (*,*) 'Particle repositioned   = ',loginfo%particle_lost
   WRITE (*,*) 'Aperture error          = ',beta%part_err

   IF (beta%output) THEN
	  CLOSE (unit=9)
   END IF
   
   IF (beta%output) THEN
	  CLOSE (unit=10)
   END IF
    
END SUBROUTINE Fr_main 
