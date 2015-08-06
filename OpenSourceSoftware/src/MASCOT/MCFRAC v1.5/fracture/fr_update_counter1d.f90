!v.02  
! fr_update_counter1d.f90
!
!  Subroutine fr_update_counter2d when the time_new < time final

SUBROUTINE fr_update_counter1d 

	USE mod_global
	USE mod_dynamic_allocation

! Variables declaration

	IMPLICIT NONE

	INTEGER:: i
	REAL:: wp
	REAL:: t_zone_new, t_zone_old
	REAL,POINTER:: time_new,x_zone_old
	INTEGER,POINTER:: ikindf
	LOGICAL:: last_zone	
	REAL:: dx,dt

 	time_new => rt_part%new%time
	t_zone_old = rt_part%old%time
	x_zone_old => rt_part%old%x 
	ikindf => rt_part%old%ikind
	last_zone=.false. !Default set of last_zone

	IF (time_new.lt.t_zone_old) THEN
		WRITE (*,*) 'ERROR! sampling time, time_new=',time_new,' t_zone_old=',t_zone_old
	END IF
	IF ((x_zone_old.lt.id_zones(izone)%W%border).OR.(x_zone_old.gt.id_zones(izone)%E%border)) THEN
		WRITE (*,*) 'position error: counter 1D '
		WRITE (*,*) 'id_zones(izone)%W%border:,',id_zones(izone)%W%border
		WRITE (*,*) 'id_zones(izone)%E%border:,',id_zones(izone)%E%border
		WRITE (*,*) 'izone=',izone
		WRITE (*,*) 'rt_part%old%x=',rt_part%old%x
	END IF	

   ! Body of the subroutine

 cycleonzone: DO	
			!found where the particle is going on

			IF (mu.GE.0) THEN
				dx=id_zones(izone)%E%border-x_zone_old
				fly_d='E'
                IF (dx.lt.0) THEN
                   WRITE (*,*) 'Ex dx=',dx,'id_zones(izone)%E%border= ',id_zones(izone)%E%border,'x_zone_old=',x_zone_old
                   WRITE (*,*) 'mu=',mu
                END IF
			ELSE
				dx=x_zone_old-id_zones(izone)%W%border
				fly_d='W'
                IF (dx.lt.0) THEN
                   WRITE (*,*) 'Wx dx=',dx,'id_zones(izone)%W%border= ',id_zones(izone)%W%border,'x_zone_old=',x_zone_old
                END IF
			END IF

            IF (dx.lt.0) dx=0

			!after this IF I know where the particle is going on, you to?
			
			!process the next zone 
			
			!Update position

			SELECT CASE (TRIM(fly_d))
				CASE ('W')
					dt=dx/(ABS(mu)*rt_part%old%fracture%velocity%value)
             		t_zone_new=t_zone_old+dt
                    IF (t_zone_new.GT.time_new) THEN
						last_zone=.TRUE.
						t_zone_new=time_new
					ELSE
						x_zone_old=>id_zones(izone)%W%border
					END IF

					CALL update_channal
					
					IF (last_zone) THEN
						EXIT cycleonzone
					END IF 	
					
					izone=id_zones(izone)%W%izone
				
					IF (id_zones(izone)%zonekind.EQ.0) THEN !the particle is in a black hole zone!
						lost_particle=.TRUE.
						num_particles%W=num_particles%W+1
				    	particle_counts(ikindf,izone,I_chan_old) =      &
&           	particle_counts(ikindf,izone,I_chan_old)+width_timech		
						EXIT cycleonzone
					END IF
				CASE ('E')
					dt=dx/(ABS(mu)*rt_part%old%fracture%velocity%value)
                	t_zone_new=t_zone_old+dt
    				IF (t_zone_new.GT.time_new) THEN
						last_zone=.TRUE.
						t_zone_new=time_new
					ELSE
						x_zone_old=>id_zones(izone)%E%border
					END IF

					CALL update_channal
					
					IF (last_zone) THEN
						EXIT cycleonzone
					END IF 
					
					izone=id_zones(izone)%E%izone
				
					IF (id_zones(izone)%zonekind.EQ.0) THEN !the particle is in a black hole zone!
						lost_particle=.TRUE.
						num_particles%E=num_particles%E+1
	            	particle_counts(ikindf,izone,I_chan_old) =      &
&           		particle_counts(ikindf,izone,I_chan_old)+width_timech					
						EXIT cycleonzone
					END IF

				CASE DEFAULT
					WRITE (*,*) 'How many cardinal points do you know in 1D?'
					WRITE (*,*) 'probabily the great programmator has done a mistake...'
			END SELECT
			
	END DO cycleonzone
	
	IF (l_writelog) THEN
		WRITE (99,*) 'fr_update_counter1d'
	END IF
	
    RETURN

	CONTAINS 

	SUBROUTINE update_channal

	   IF (t_zone_new.LT.t_zone_old) THEN          !DEBUG
		   WRITE (*,*) 'ERROR! update_channal'      !DEBUG
		   STOP                                     !DEBUG
	   END IF
    
      !compute the new time channel and check

      IF (t_zone_new.GE.time_sim_end) THEN
         I_chan_new=n_timech		! to prevent integer overflow the
				        ! max value of time channel is assigned
				        ! when the sampled time is greater than
				        !the end of the simulation!
         t_zone_new=time_sim_end
		 last_zone=.TRUE.
		 lost_particle=.TRUE.
      ELSE
         I_chan_new=t_zone_new/width_timech+1         !compute the i_chan_new 
      END IF

      IF (I_chan_new.EQ.I_chan_old) THEN
         
         wp=(t_zone_new-t_zone_old)	!compute weight of the particle
         
         if (wp.lt.0) then                            !DEBUG
		    write (*,*) 'B: counter warning? wp=',wp
	    	write (*,*) 'I_chan_new=',I_chan_new,' t_zone_new=',t_zone_new
	        write (*,*) 'I_chan_old=',I_chan_old,' t_zone_old=',t_zone_old
         end if	                                    !DEBUG
         
         ! Update counter
         particle_counts(ikindf,izone,I_chan_old) = & 
&			particle_counts(ikindf,izone,I_chan_old) + wp
 
      ELSE
      
         wp=(T_right_count(I_chan_old)-t_zone_old)	!compute weight of the particle
				                                    !for the current timechannal 	
         if (wp.lt.0) then                             !DEBUG
	     	write (*,*) 'C: counter warning? wp=',wp
			write (*,*) 'I_chan_new=',I_chan_new,' t_zone_new=',t_zone_new
			write (*,*) 'I_chan_old=',I_chan_old,' t_zone_old=',t_zone_old
			write (*,*) 'T_right_count(I_chan_old)=',T_right_count(I_chan_old)
			write (*,*) 'num_particles%in=',num_particles%in
		 end if	                                     !DEBUG

        !Update counter 					
        particle_counts(ikindf,izone,I_chan_old) = & 
&       particle_counts(ikindf,izone,I_chan_old) + wp 

		DO i=(I_chan_old+1),(I_chan_new-1)		!Update channals between
                                                !I_chan_old+1 and I_chan_new-1
            particle_counts(ikindf,izone,I) =      &
&           particle_counts(ikindf,izone,I)+width_timech
        END DO

        !Update the last channal
        wp=(t_zone_new-T_right_count(I_chan_new-1))	!compute weight of the particle
         
        if (wp.lt.0) then
            write (*,*) 'D: counter warning? wp=',wp
            write (*,*) 'I_chan_new=',I_chan_new,' t_zone_new=',t_zone_new
            write (*,*) 'I_chan_old=',I_chan_old,' t_zone_old=',t_zone_old
            write (*,*) 'T_right_count(I_chan_new-1)=',T_right_count(I_chan_new-1)
        end if	

        !Update counter
        particle_counts(ikindf,izone,I_chan_new)= &
&       particle_counts(ikindf,izone,I_chan_new)+wp 

     END IF
     
     ! update variables

     t_zone_old=t_zone_new
     I_chan_old=I_chan_new

   END SUBROUTINE update_channal

END SUBROUTINE fr_update_counter1d


