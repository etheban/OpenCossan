!	file:	mod_global.f90 V.01
!
!	This module contains all variables sheared in the entire project
!	The "auxiliar variables" are variables that can be derived from other variables
!	The "runtime variables" are variables that change their values during the MC simulation

MODULE mod_global

	IMPLICIT NONE

!************************************************************************
!						SETTING VARIABLES								*
!************************************************************************

! Not present in standalone fracture Mascot code
!	CHARACTER (LEN = 10)::	c_source_kind	!set the kind of source
!	CHARACTER (LEN = 10)::	c_kindsim		!set the kind of transition
!	CHARACTER (LEN = 10)::	c_output		!set the kind of the output

	INTEGER::	iter_method	! kind of the iteration method 

	!	Monte Carlo parametres

	INTEGER::	ntot_reactions		!Total number of reaction rates	
	INTEGER::	ntot_movements		!Total number of movement rates	
	INTEGER::	ntot_nonlinearity	!Total number of nonlinear rates

	INTEGER::	iseed				!Seed for the random variables

	INTEGER::	n_kind	!total number of particle kinds

	LOGICAL::	l_border	! Automatic border of the cell (T) or from file (F)
	INTEGER::	n_spacedim	! space dimension of the domain (Geometry)
	REAL::		spece_step  ! cell dimension 

	REAL, TARGET::	time_sim_start	!Initatial time of the simulation
	REAL, TARGET::	time_sim_end	!Final time of the simulation
!************************************************************************
!	Flags
!************************************************************************

	LOGICAL::   l_writelog			! flag for log file (T=YES)	
	LOGICAL::   l_variance			! flag for variance (T=YES)	
	LOGICAL::   l_correlation		! enable correlation (T=YES)
	LOGICAL::   l_corr_source		! enable correlation of source (T=YES)	
	
!************************************************************************
!				VARIABLES FOR THE LINEAR SIMULATION						*
!************************************************************************

	INTEGER::	izone		! Zone number  (run time variable)
	INTEGER::	ikind		! Kind of particle (run time variable)
	INTEGER::	isource		! Number of source trial (run time variable)
	INTEGER::	ipartlost	! Kind of particle not followed in the simulation
	REAL::		sigma_tot	! total cross-section 
	REAL::		sigma_tot_inv	! total cross-section^-1 
			
	INTEGER::   n_bc		! number of boudary kind

!	REAL::	time_old		! Run time of the particle
!	REAL::	time_new		! Time of the "next" particle transition

!	REAL,TARGET:: time_start	! Initail time of linear simulation	
!	REAL,TARGET:: time_end		! Final time of linear simulation
	
	TYPE pos_f
		REAL:: x
		REAL:: y
!		REAL:: z
	END TYPE pos_f	

	TYPE rt_fr1
		REAL:: value
		INTEGER:: bin
	END TYPE rt_fr1


	TYPE rt_fr2
		TYPE (rt_fr1):: orientation
		TYPE (rt_fr1):: velocity
		TYPE (rt_fr1):: length
		TYPE (rt_fr1):: aperture
		TYPE (pos_f):: location !start point of the fracture	
	END TYPE rt_fr2

	TYPE rtp1_type
			INTEGER:: ikind
			REAL:: time
			REAL:: x
			REAL:: y
!			REAL:: z
			TYPE(rt_fr2):: fracture
	END TYPE rtp1_type
			
			
	TYPE rtp2_type
			TYPE (rtp1_type):: old
			TYPE (rtp1_type):: new
	END TYPE rtp2_type
					
	TYPE(rtp2_type),TARGET:: rt_part

	INTEGER:: ibin
	
!************************************************************************
!							VARIABLES FOR COUNTERS						*
!************************************************************************

	INTEGER::	n_zone			! total number of zones
	INTEGER::	n_timech		! Number of time channel (for the particles couter)
	INTEGER::	I_chan_old		! Time channel that correspon to time_old (auxiliar variables)
	INTEGER::	I_chan_new		! Time channel that correspon to time_new (auxiliar variables)

	TYPE count_par
		INTEGER:: in
		INTEGER:: dead
		INTEGER:: aborted_source
		INTEGER:: aborted_rf
		INTEGER:: E
		INTEGER:: W
		INTEGER:: N
		INTEGER:: S
		INTEGER:: U
		INTEGER:: D
	END TYPE count_par

	TYPE (count_par):: num_particles

	REAL::  part_weight		! particle weight (useful to restart the computation from any intermided time)

	REAL::	width_timech	! Width of time channel for particle counter (auxiliar variables)
	REAL::	width_timech2	! square width of time channel for particle counter (auxiliar variables)


	TYPE t_beta
		CHARACTER(LEN = 32):: filename
		LOGICAL:: output    !flag for the compute of beta
		LOGICAL:: bfp       !flag for the check of first passage trought a given zone
		REAL:: value        !current value of beta
		REAL:: x            !control panel position (x)
		REAL:: y            !control panel position (y)
		INTEGER:: part_err  !number of error in aperture
	END TYPE t_beta

	TYPE (t_beta):: beta,tau ! parameter to correlate aperture and travel time

!************************************************************************
!				VARIABLES FOR THE FRACTURE NETWORK			    		*
!************************************************************************

	TYPE par_cdf1
		INTEGER:: nbins
		CHARACTER (LEN=3) :: kind
		CHARACTER (LEN=64):: filename
	END TYPE par_cdf1

	!The kind of cdf can be: LIN (linear), LOG (logaritmic)

	TYPE par_cdf2
		TYPE (par_cdf1):: velocity
		TYPE (par_cdf1):: orientation
		TYPE (par_cdf1):: length
		TYPE (par_cdf1):: aperture
		TYPE (par_cdf1):: source_time
		TYPE (par_cdf1):: source_vel
		TYPE (par_cdf1):: source_ori
		TYPE (par_cdf1):: source_x
		TYPE (par_cdf1):: source_y
	END TYPE par_cdf2

	TYPE (par_cdf2),TARGET:: cdf

	!Runtime variable

	LOGICAL:: l_initialstate,l_source,lost_particle
	CHARACTER (LEN=2):: fly_d


			
		TYPE f_def
			CHARACTER (LEN=3)::dist_kind
			REAL:: mean
			REAL:: std
			REAL:: span			
		END TYPE f_def
				
		TYPE rt_frac
				TYPE (f_def):: orientation	 !fracture orientation
				TYPE (f_def):: velocity !fracture velocity
				TYPE (f_def):: length	 !fracture length
				TYPE (f_def):: aperture	 !fracture aperture		
		END TYPE rt_frac	

	TYPE(rt_frac),TARGET:: fr_property

	REAL:: lambda ! tmp variable to test the decay 

	REAL:: mu ! cos(theta) variable used to speed-up the simulation
	REAL:: nu !sin(thata) variable used to speed-up the simulation
	
	TYPE logw
		INTEGER:: particle_lost
    END TYPE logw		
	
	TYPE (logw):: loginfo			

!************************************************************************
!				VARIABLES FOR THE PARTICLE SOURCE		    		*
!***********************************************************************	

		TYPE skind
			REAL:: par1
			REAL:: par2
			CHARACTER (LEN=6):: kind
		END TYPE skind


		TYPE source_type
			INTEGER:: ikind 
			INTEGER:: ntrials
			TYPE (skind):: x,y,time,velocity,orientation
		END TYPE source_type
			
		TYPE (source_type),TARGET:: source
			

			
!************************************************************************
!							PRE PROCESSING								*
!************************************************************************

	INTEGER::	iunit	! open unit (for input file and output file)

!************************************************************************
!							POST PROCESSING								*
!************************************************************************

	CHARACTER (LEN=70), DIMENSION (2):: c_title	!contains the title of the current simulation

	CHARACTER (LEN = 16):: output_filename1		! name of the output file 1

!	CHARACTER (LEN = 16):: output_filename2		! name of the output file 2


!************************************************************************
!							Explicit INTERFACES							*
!************************************************************************

  INTERFACE openfile

   SUBROUTINE openfile (iunit,pathname,filename,filestatus)
		INTEGER:: iunit
		CHARACTER(*):: pathname,filename,filestatus
    END SUBROUTINE openfile

  END INTERFACE openfile

END MODULE mod_global
