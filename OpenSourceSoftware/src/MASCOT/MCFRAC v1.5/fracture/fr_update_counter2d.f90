!v.01  
!  fr_update_counter2d.f90
!
!  Subroutine fr_update_counter2d when the time_new < time final

!
! WARNING: no weights!!!!
!

SUBROUTINE fr_update_counter2d 

	USE mod_global

	USE mod_dynamic_allocation

! Variables declaration
! copiare da 1d

	IMPLICIT NONE

	INTEGER:: i
	REAL:: wp
	REAL:: t_zone_new, t_zone_old, y_zone_new,x_zone_new
	REAL,POINTER:: time_new,x_zone_old,y_zone_old	
	INTEGER,POINTER:: ikindf
	LOGICAL:: last_zone	

	REAL:: dx,dy,dt

	time_new => rt_part%new%time
	
	t_zone_old =  rt_part%old%time
	x_zone_old => rt_part%old%x 
	y_zone_old => rt_part%old%y
	 
	ikindf => rt_part%old%ikind
			
	last_zone=.false. !Default set of last_zone

	IF (time_new.le.t_zone_old) THEN
		WRITE (*,*) 'ERROR! sampling time, time_new=',time_new,' t_zone_old=',t_zone_old
	END IF


	! trovare zone di arrivivo dalla mesh!

	! go forward (E) and backward (W)

	! up and down

 ! Body of the subroutine

 cycleonzone: DO		
 
			!idea :-) usare equazioni rette per trovare punto incontro	
		
			!found where the particle is going on

			IF (mu.GE.0) THEN
				dx=id_zones(izone)%W%border-x_zone_old
				IF(nu.GE.0) THEN
				    dy=id_zones(izone)%N%border-y_zone_old
					IF (dx.GT.dy) THEN
						fly_d='N'
					ELSE
						fly_d='E'
					END IF
				ELSE
				    dy=y_zone_old-id_zones(izone)%S%border
					IF (dx.GT.dy) THEN
						fly_d='S'
					ELSE
						fly_d='E'
					END IF
				END IF
			ELSE
				dx=x_zone_old-id_zones(izone)%E%border
				IF(nu.GE.0) THEN
				    dy=id_zones(izone)%N%border-y_zone_old
					IF (dx.GT.dy) THEN
						fly_d='N'
					ELSE
						fly_d='W'
					END IF
				ELSE
			        dy=y_zone_old-id_zones(izone)%S%border
					IF (dx.GT.dy) THEN
						fly_d='S'
					ELSE
						fly_d='W'
					END IF
				END IF
			END IF
			!after this IF I know where the particle is going on, you too?
			
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
						x_zone_new=id_zones(izone)%W%border
						y_zone_new=y_zone_old+dt*(nu*rt_part%old%fracture%velocity%value)	
					END IF

					CALL update_channal
					
					IF (last_zone) EXIT cycleonzone	
					
					izone=id_zones(izone)%W%izone
				
					IF (id_zones(izone)%zonekind.EQ.0) THEN !the particle is in a black hole zone!
						lost_particle=.TRUE.
						num_particles%W=num_particles%W+1
						EXIT cycleonzone
					END IF
				CASE ('E')
					dt=dx/(ABS(mu)*rt_part%old%fracture%velocity%value)
					t_zone_new=t_zone_old+dt
    				IF (t_zone_new.GT.time_new) THEN
						last_zone=.TRUE.
						t_zone_new=time_new
					ELSE
						x_zone_new=id_zones(izone)%E%border
						y_zone_new=y_zone_old+dt*(nu*rt_part%old%fracture%velocity%value)	
					END IF

					CALL update_channal
					
					IF (last_zone) EXIT cycleonzone	
					
					izone=id_zones(izone)%E%izone
				
					IF (id_zones(izone)%zonekind.EQ.0) THEN !the particle is in a black hole zone!
						lost_particle=.TRUE.
						num_particles%E=num_particles%E+1
						EXIT cycleonzone
					END IF

				CASE ('N')
					dt=dy/(ABS(nu)*rt_part%old%fracture%velocity%value)
					t_zone_new=t_zone_old+dt
    				IF (t_zone_new.GT.time_new) THEN
						last_zone=.TRUE.
						t_zone_new=time_new
					ELSE
						x_zone_new=x_zone_old+dt*(mu*rt_part%old%fracture%velocity%value)
						y_zone_new=id_zones(izone)%N%border	
					END IF

					CALL update_channal
					
					IF (last_zone) EXIT cycleonzone	
					
					izone=id_zones(izone)%N%izone
				
					IF (id_zones(izone)%zonekind.EQ.0) THEN !the particle is in a black hole zone!
						lost_particle=.TRUE.
						num_particles%N=num_particles%N+1
						EXIT cycleonzone
					END IF
				
				CASE ('S')
					dt=dy/(ABS(nu)*rt_part%old%fracture%velocity%value)
					t_zone_new=t_zone_old+dt
    				IF (t_zone_new.GT.time_new) THEN
						last_zone=.TRUE.
						t_zone_new=time_new
					ELSE
						x_zone_new=x_zone_old+dt*(mu*rt_part%old%fracture%velocity%value)
						y_zone_new=id_zones(izone)%S%border	
					END IF

					CALL update_channal
					
					IF (last_zone) EXIT cycleonzone	
					
					izone=id_zones(izone)%S%izone
				
					IF (id_zones(izone)%zonekind.EQ.0) THEN !the particle is in a black hole zone!
						lost_particle=.TRUE.
						num_particles%S=num_particles%S+1
						EXIT cycleonzone
					END IF				

				CASE DEFAULT
					WRITE (*,*) 'How many cardinal points do you know?'
					WRITE (*,*) 'probabily the great programmator has done a mistake...'
			END SELECT
			
	END DO cycleonzone
	
    RETURN

	CONTAINS 

	SUBROUTINE update_channal

	   IF (t_zone_new.LT.t_zone_old) THEN          !DEBUG
		   WRITE (*,*) 'ERROR! update_channal'      !DEBUG
		   WRITE (*,*) 'mu=',mu
		   Write (*,*) 'Dx=',ABS(x_zone_new-x_zone_old)
			Write (*,*)	't_zone_new=',t_zone_new,' t_zone_old=',t_zone_old
			write (*,*) rt_part%new
			Write (*,*) rt_part%old
		   pause                                    !DEBUG
	   END IF
	   
	   !Another if debug cycle
	   
	   IF (i_chan_new.LT.i_chan_old) THEN
		Write (*,*) 'Error in update channel'
		Write (*,*) 'i_chan_new =',i_chan_new
		Write (*,*) 'i_chan_old =',i_chan_old
	   END IF                                      !DEBUG
    
      !compute the new time channel and check

      IF (t_zone_new.GE.time_sim_end) THEN
         I_chan_new=n_timech				! to prevent integer overflow the
				                              ! max value of time channel is assigned
				                              ! when the sampled time is greater than
				                              !the end of the simulation!
         t_zone_new=time_sim_end
      ELSE
         I_chan_new=t_zone_new/width_timech+1         !compute the i_chan_new 
         IF (I_chan_new.GT.n_timech) THEN             !DEBUG
			write (*,*) 'B: ERROR'                 
			write (*,*) 'I_chan_new=',I_chan_new,' t_zone_new=',t_zone_new
			write (*,*) 'time_sim_end=',time_sim_end			
         END IF                                       !DEBUG
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

END SUBROUTINE fr_update_counter2d


