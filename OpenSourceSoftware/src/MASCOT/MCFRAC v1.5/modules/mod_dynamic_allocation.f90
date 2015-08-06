!
!	file:	mod_dynamic_allocation.f90
!
!	Module: mod_dynamic_allocation
!

MODULE mod_dynamic_allocation

!	PARAMETER max_nl_par=2
!
	IMPLICIT NONE

!	variable for the contaminant rates

!	num_rate(n_kind):	number of transition reactions and the number
!						of movements for each kind of particle

	TYPE desc_rate
		INTEGER::n_reactions,n_movements,n_nonlinearity
	END TYPE desc_rate

	TYPE (desc_rate), DIMENSION (:),TARGET,ALLOCATABLE:: num_rate

!	t_rate(n_zones,n_reactions_tot)
	
	TYPE tran_rate 
		REAL::		rate
		INTEGER::	to_kind
	END TYPE tran_rate

	TYPE (tran_rate), DIMENSION (:,:),TARGET,ALLOCATABLE:: t_rate

!	m_rate(n_zones,n_movements_tot)
!
	TYPE move_rate
		REAL::		rate
		INTEGER::	to_zone
	END TYPE move_rate
!
	TYPE (move_rate), DIMENSION (:,:),TARGET,ALLOCATABLE:: m_rate
!
	!Total Transition rate: tot_tran_rate(n_kind,n_zone)
	REAL,TARGET, ALLOCATABLE::	tot_tran_rate(:,:) 



!************************************************************************
!				CDF FOR THE FRACTURES NETWORKS
!************************************************************************

	!cdf (n_bins of the current cdf,Nx bins,Nv' binds,Nt' bins) 

	REAL, DIMENSION (:,:,:,:),TARGET,ALLOCATABLE:: cdf_velocity
	REAL, DIMENSION (:,:,:,:),TARGET,ALLOCATABLE:: cdf_orientation
	REAL, DIMENSION (:,:,:,:),TARGET,ALLOCATABLE:: cdf_length
	REAL, DIMENSION (:,:,:,:),TARGET,ALLOCATABLE:: cdf_aperture

	!cdf source_* (n_bins,n_dims+1) the +1 needs to set the * values of cdf	
	
	REAL, DIMENSION (:,:),TARGET,ALLOCATABLE:: source_velocity
	REAL, DIMENSION (:,:),TARGET,ALLOCATABLE:: source_orientation
	REAL, DIMENSION (:,:),TARGET,ALLOCATABLE:: source_time
	REAL, DIMENSION (:,:),TARGET,ALLOCATABLE:: source_x
	REAL, DIMENSION (:,:),TARGET,ALLOCATABLE:: source_y
!   REAL, DIMENSION (:,:),TARGET,ALLOCATABLE:: source_z

	!Pointer for the sampling from cdfs
	REAL,DIMENSION(:),POINTER:: p_cdf
	REAL,DIMENSION(:),POINTER:: p_bin

!************************************************************************
!				VARIABLES FOR THE GEOMETRIES							*
!************************************************************************
!	variable for the geometries

	TYPE idzv 
		INTEGER:: izone	 !identification number of the adjacent zone 
		REAL::    border
	END TYPE idzv

	TYPE idzd
		INTEGER:: zonekind
		TYPE (idzv):: E,W		   ! 1D direction
		TYPE (idzv):: N,S       ! 2D direction
		TYPE (idzv):: U,D       ! 3D direction
	END TYPE idzd
	
	TYPE(idzd),DIMENSION(:),TARGET,ALLOCATABLE::  id_zones 

! Not present in standalone fracture Mascot code

!	variable for Boundary contition

!	TYPE bctype
!		INTEGER:: bc_kind
!		INTEGER:: bc_particle
!		INTEGER:: bc_pzone
!		INTEGER:: bc_pkind
!		REAL::    bc_tstart
!		REAL::	 bc_tstop
!		REAL::    bc_rate
!		REAL::    bc_c0
!	END TYPE bctype

!	TYPE (bctype), DIMENSION(:), ALLOCATABLE, TARGET:: boudary_condition
			

			
			
!************************************************************************
!				VARIABLES FOR THE NON LINEARITY						         	*
!************************************************************************

! Not present in standalone fracture Mascot code

!	TYPE nonlinear
!		REAL,DIMENSION(max_nl_par)::	parameters
!		REAL::		nlratezero		!Initail value of the nl rate
!		INTEGER::	k_owner			!particle kind that undergo to this nl rate
!		INTEGER::	k_dependent		!particle of which depend the nl rate
!		INTEGER::	nltype			!type of non linear transition
!		INTEGER::	itrans			!number of the transition that is non linerar
!	END TYPE nonlinear
!
!	nl_par(n_zones,ntot_nonlinearity)
!
!	TYPE (nonlinear), DIMENSION (:,:),TARGET,ALLOCATABLE:: nl_par
!
!	!Error of non linear iterations (n_zone)
!	REAL, DIMENSION (:,:), ALLOCATABLE::	rie
!
!	! lower and higher limits of the nonlinear variation 
!	TYPE limits
!		REAL:: lower
!		REAL:: higher
!		REAL:: iteration
!	END TYPE limits
!
!	TYPE (limits), DIMENSION (:), ALLOCATABLE:: th_nl !th_nl(ntot_nonlinearity)	
!
!	! Number of zone in which the rate variation ecxiding drate
!	TYPE overstep
!		INTEGER:: lower
!		INTEGER:: higher
!		INTEGER:: iteration
!	END TYPE overstep
!
!	TYPE(overstep), DIMENSION (:), ALLOCATABLE:: i_overstep

! ***********************************************************************
! *								counters								*
! ***********************************************************************

! Not present in standalone fracture Mascot code
!
!	! counter for the non linear rate 
!	! Rate of the non linear rates (n_zone,nl_step,ntot_nonlinearity) 
!	REAL, DIMENSION (:,:,:), TARGET, ALLOCATABLE::	nl_rate

	!Expected value of the particle present in each time channel (its the main counter of the program GRACOF)
	!particle_counts(n_kind,n_zone,n_count_tstep)

	REAL, ALLOCATABLE::		particle_counts(:,:,:) 

	!Expected value of the (particle)^2 present in each time channel (its the main counter of the program GRACOF)
	!particle_counts(n_kind,n_zone,n_count_tstep)

	REAL, ALLOCATABLE::		particle_counts2(:,:,:) 

! Not present in standalone fracture Mascot code
!
!	!Expected value of the particle present in each time channel (its the counter for NLS)
!	!particle_counts(n_kind,n_zone,n_count_tstep)
!
!	REAL, ALLOCATABLE::		particle_counts_nl(:,:,:) 
!
!	!Expected value of the (particle)^2 present in each time channel (its the counter for NLS)
!	!particle_counts(n_kind,n_zone,n_count_tstep)
!
!	REAL, ALLOCATABLE::		particle_counts2_nl(:,:,:) 
!
!	!Count of particle present at the end of each time step (in non linear simulation)
!	!i_particle_counts (n_kind,n_zone,nl_step)
!	INTEGER,ALLOCATABLE, TARGET, DIMENSION (:,:,:):: i_particle_counts
!
!	!Count of particle present at the end of each iteration (in non linear simulation)
!	!i_particle_iteration (n_kind,n_zone,nmax_iteration)
!	INTEGER,ALLOCATABLE, TARGET, DIMENSION (:,:,:):: i_particle_iteration
!
!	!Count of particle present at the end of each iteration (in non linear simulation)
!	!i_particle_iteration (n_zone,nmax_iteration,ntot_nonlinearity)
!
!	REAL,ALLOCATABLE, TARGET, DIMENSION (:,:,:):: rate_iteration

	!Time Right of the time channel: t_right_count (n_count_time)
	REAL, ALLOCATABLE::	t_right_count(:) 
	REAL, ALLOCATABLE:: x_right_count(:)

!	!number of particles entered in the system from the source and the initial state
!	REAL, ALLOCATABLE::		in_particle (:)

	!Variance of the particle concentration in the zone and time
	!var(n_kind,n_zone,n_count_tstep)

	REAL, ALLOCATABLE::	var(:,:,:) 

END MODULE mod_dynamic_allocation



