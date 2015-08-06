! this subroutine allacate the memory for the matrix and vector

SUBROUTINE allocate_memory 

! Use modules

!	USE mod_pointer

	USE mod_dynamic_allocation

	USE mod_global

! Variables declaration

	IMPLICIT NONE

	INTEGER:: ierr

	REAL:: all_mem,	tot_all_mem 

	CHARACTER (LEN = 16):: c_matrix_name

!	Body of the subroutine allocate_memory

!	Initialize variables

	tot_all_mem=0


		! (2)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			ALLOCATE (id_zones(n_zone),STAT = ierr) 

			! Compute the allocated memory 
		
			all_mem=SIZE(id_zones)*	(KIND(id_zones%zonekind)+6*KIND(id_zones%E%izone)+6*KIND(id_zones%E%border))

			! Check the allocated status

			c_matrix_name="id_zones"

			CALL all_write_status


		! (6)	Allocate the matrix defined in the module "mod_dynamic_allocation"

		   ALLOCATE (particle_counts(n_kind,n_zone,0:n_timech),STAT = ierr)

		   ! Set the initial value of the allocated matrix
		
			particle_counts=0
	
	      ! Compute the allocated memory 

			all_mem=SIZE(particle_counts)*KIND(particle_counts)

			! Check the allocated status

			c_matrix_name="particle_counts"

			CALL all_write_status

		
      	! (7)	Allocate the matrix defined in the module "mod_dynamic_allocation"

   		IF (l_variance) THEN
   		
   			ALLOCATE (particle_counts2(n_kind,n_zone,0:n_timech),STAT = ierr)

   			! Set the initial value of the allocated matrix
 		
   			particle_counts2=0
	
   			! Compute the allocated memory 

   			all_mem=SIZE(particle_counts2)*KIND(particle_counts2)

   			! Check the allocated status

   			c_matrix_name="particle_counts2"

			   CALL all_write_status

        END IF
        
		! (8)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			ALLOCATE (T_right_count(n_timech),STAT = ierr)

			! Set the initial value of the allocated matrix
		
			T_right_count=0
	
			! Compute the allocated memory 

			all_mem=SIZE(T_right_count)*KIND(T_right_count)

			! Check the allocated status

			c_matrix_name="T_right_count"

			CALL all_write_status


		! (1.10)	Allocate the matrix defined in the module "mod_dynamic_allocation"

      IF (l_variance) THEN
			
			ALLOCATE (var(n_kind,n_zone,n_timech),STAT = ierr)

			! Set the initial value of the allocated matrix
		
			var=0
	
			! Compute the allocated memory 

			all_mem=SIZE(var)*KIND(var)

			! Check the allocated status

			c_matrix_name="var"

			CALL all_write_status
			
		END IF
		
		! (1.11)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			ALLOCATE (x_right_count(n_zone),STAT = ierr)

			! Set the initial value of the allocated matrix
		
			x_right_count=0
	
			! Compute the allocated memory 

			all_mem=SIZE(x_right_count)*KIND(x_right_count)

			! Check the allocated status

			c_matrix_name="x_right_count"

			CALL all_write_status			

		! (3.1)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			IF (l_correlation) THEN
				ALLOCATE (cdf_velocity(cdf%velocity%nbins,2,cdf%velocity%nbins,cdf%orientation%nbins),STAT = ierr) 
			ELSE
				ALLOCATE (cdf_velocity(cdf%velocity%nbins,2,1,1),STAT = ierr) 
			END IF

			! Set the initial value of the allocated matrix
		
			cdf_velocity=0

			! Compute the allocated memory 
		
			all_mem=SIZE(cdf_velocity)*	(KIND(cdf_velocity))

			! Check the allocated status

			c_matrix_name="cdf_velocity"

			CALL all_write_status
	
		! (3.2)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			IF (l_correlation) THEN
				ALLOCATE (cdf_orientation(cdf%orientation%nbins,cdf%velocity%nbins+1,cdf%velocity%nbins,cdf%orientation%nbins),STAT = ierr)
			ELSE
				ALLOCATE (cdf_orientation(cdf%orientation%nbins,2,1,1),STAT = ierr) 
			END IF
			! Set the initial value of the allocated matrix
		
			IF (ALLOCATED(cdf_orientation)) THEN
				cdf_orientation=0
			END IF
			
			! Compute the allocated memory 
		
			all_mem=SIZE(cdf_orientation)*(KIND(cdf_orientation))

			! Check the allocated status

			c_matrix_name="cdf_orientation"

			CALL all_write_status

		! (3.3a)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			IF (l_correlation) THEN
				IF (fr_property%length%dist_kind.EQ.'lmv') THEN
					ALLOCATE (cdf_length(1,1,cdf%velocity%nbins,cdf%orientation%nbins),STAT = ierr) 
				ELSE
					ALLOCATE (cdf_length(cdf%length%nbins,2,cdf%velocity%nbins,cdf%orientation%nbins),STAT = ierr) 
				ENDIF
			ELSE
				ALLOCATE (cdf_length(cdf%length%nbins,2,1,1),STAT = ierr) 
			END IF

			! Set the initial value of the allocated matrix
		
			cdf_length=0

			! Compute the allocated memory 
		
			all_mem=SIZE(cdf_length)*	(KIND(cdf_length))

			! Check the allocated status

			c_matrix_name="cdf_length"

			CALL all_write_status

	

		IF (beta%output) THEN

			! (3.4)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			IF (l_correlation) THEN
				ALLOCATE (cdf_aperture(cdf%aperture%nbins,2,cdf%velocity%nbins,cdf%orientation%nbins),STAT = ierr) 
			ELSE
				ALLOCATE (cdf_aperture(cdf%aperture%nbins,2,1,1),STAT = ierr) 
			END IF


			! Set the initial value of the allocated matrix
		
			cdf_aperture=0

			! Compute the allocated memory 
		
			all_mem=SIZE(cdf_aperture)*	(KIND(cdf_aperture))

			! Check the allocated status

			c_matrix_name="cdf_aperture"

			CALL all_write_status

		END IF

		! allocate memory for the sources

		IF (source%velocity%kind.EQ.'cdf') THEN

			! (3.5)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			ALLOCATE (source_velocity(cdf%source_vel%nbins,2),STAT = ierr) 

			! Set the initial value of the allocated matrix
		
			source_velocity=0

			! Compute the allocated memory 
		
			all_mem=SIZE(source_velocity)*	(KIND(source_velocity))

			! Check the allocated status

			c_matrix_name="source_velocity"

			CALL all_write_status

		END IF
		
		IF (source%orientation%kind.EQ.'cdf') THEN

			! (3.6)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			IF (l_corr_source) THEN
				ALLOCATE (source_orientation(cdf%source_ori%nbins,cdf%source_vel%nbins+1),STAT = ierr) 
			ELSE
				ALLOCATE (source_orientation(cdf%source_ori%nbins,2),STAT = ierr) 
			END IF
			! Set the initial value of the allocated matrix
		
			source_orientation=0

			! Compute the allocated memory 
		
			all_mem=SIZE(source_orientation)*	(KIND(source_orientation))

			! Check the allocated status

			c_matrix_name="source_orientation"

			CALL all_write_status

		END IF

		IF (source%x%kind.EQ.'cdf') THEN

			! (3.7)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			ALLOCATE (source_x(cdf%source_x%nbins,2),STAT = ierr) 

			! Set the initial value of the allocated matrix
		
			source_x=0

			! Compute the allocated memory 
		
			all_mem=SIZE(source_x)*	(KIND(source_x))

			! Check the allocated status

			c_matrix_name="source_x"

			CALL all_write_status

		END IF

		IF (source%y%kind.EQ.'cdf') THEN

			! (3.8)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			ALLOCATE (source_y(cdf%source_y%nbins,2),STAT = ierr) 

			! Set the initial value of the allocated matrix
		
			source_y=0

			! Compute the allocated memory 
		
			all_mem=SIZE(source_y)*	(KIND(source_y))

			! Check the allocated status

			c_matrix_name="source_y"

			CALL all_write_status

		END IF

		IF (source%time%kind.EQ.'cdf') THEN

			! (3.9)	Allocate the matrix defined in the module "mod_dynamic_allocation"

			ALLOCATE (source_time(cdf%source_time%nbins,2),STAT = ierr) 

			! Set the initial value of the allocated matrix
		
			source_time=0

			! Compute the allocated memory 
		
			all_mem=SIZE(source_time)*	(KIND(source_time))

			! Check the allocated status

			c_matrix_name="source_time"

			CALL all_write_status

		END IF


CONTAINS

	SUBROUTINE all_write_status 

	CHARACTER (LEN = 25):: c_question

!
!	Check if the array has been allocated correctly 
!

		IF (ierr.eq.0) THEN 
		
			IF (l_writelog) THEN 

			WRITE (*,*) c_matrix_name,': has been allocated'

			tot_all_mem=tot_all_mem+all_mem

			WRITE (*,*) 'MEMORY ALLOCATED:      ',tot_all_mem/1024,'kbyte'
			WRITE (*,*) 
			END IF

		ELSE
		
!			oldtc = SETTEXTCOLORRGB(#0000FF)	! RED
			WRITE (*,*) 'WARNING: ',c_matrix_name,': NOT allocated'

!			oldtc = SETTEXTCOLORRGB(#FFFFFF)	! white
			WRITE (*,*) 'Error type (RUN-TIME ERROR): ',ierr
		
			WRITE (*,*) 'Do you want continue (Y/N)?'

GET_ANSWER: DO 

				READ (*,*) c_question
				WRITE (*,*)
		
				SELECT CASE (c_question)

					CASE ("Y","y")

						EXIT GET_ANSWER
		
					CASE ("N","n")

						STOP
		
					CASE DEFAULT

						WRITE (*, *) "Command not recognized; (Y/N)"
				
				END SELECT 
				
			END DO GET_ANSWER

		END IF

	END SUBROUTINE all_write_status


END SUBROUTINE allocate_memory





 



