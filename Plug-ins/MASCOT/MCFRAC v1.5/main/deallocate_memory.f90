! this subroutine allacate the memory for the matrix and vector

SUBROUTINE deallocate_memory 

! Use modules

	USE mod_dynamic_allocation

	USE mod_global

! Variables declaration

	IMPLICIT NONE

	INTEGER:: ierr

! Body of subroutine

!	DEALLOCATE (tot_tran_rate, STAT = ierr) 
		
!	DEALLOCATE (id_zones,STAT = ierr) 

!	DEALLOCATE (num_rate,STAT = ierr) 

!	DEALLOCATE (t_rate,STAT = ierr) 

!	DEALLOCATE (m_rate,STAT = ierr) 

	DEALLOCATE (particle_counts,STAT = ierr)

	DEALLOCATE (particle_counts2,STAT = ierr)

	DEALLOCATE (t_right_count,STAT = ierr)

!	DEALLOCATE (in_particle,STAT = ierr)

	DEALLOCATE (var,STAT = ierr)

!	DEALLOCATE (i_particle_counts,STAT = ierr)

!	DEALLOCATE (counter, STAT = ierr)

!	IF (ALLOCATED(i_particle_iteration)) DEALLOCATE	(i_particle_iteration,STAT = ierr)	

!	IF (ALLOCATED(nl_par)) DEALLOCATE (nl_par,STAT = ierr)

!	IF (ALLOCATED(rie)) DEALLOCATE (rie,STAT = ierr)

!	IF (ALLOCATED(nl_rate)) DEALLOCATE (nl_rate,STAT = ierr)

!	IF (ALLOCATED(rate_iteration)) DEALLOCATE (rate_iteration,STAT = ierr)

!	IF (ALLOCATED(th_nl)) DEALLOCATE (th_nl,STAT = ierr)

	IF (ALLOCATED(cdf_velocity)) DEALLOCATE (cdf_velocity,STAT = ierr)
	IF (ALLOCATED(cdf_orientation)) DEALLOCATE (cdf_orientation,STAT = ierr)
	IF (ALLOCATED(cdf_length)) DEALLOCATE (cdf_length,STAT = ierr)
	IF (ALLOCATED(cdf_aperture)) DEALLOCATE (cdf_aperture,STAT = ierr)
	IF (ALLOCATED(source_velocity)) DEALLOCATE (source_velocity,STAT = ierr)
	IF (ALLOCATED(source_orientation)) DEALLOCATE (source_orientation,STAT = ierr)
	IF (ALLOCATED(source_x)) DEALLOCATE (source_x,STAT = ierr)
	IF (ALLOCATED(source_y)) DEALLOCATE (source_y,STAT = ierr)
	IF (ALLOCATED(source_time)) DEALLOCATE (source_time,STAT = ierr)

END SUBROUTINE deallocate_memory





 



