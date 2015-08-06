!
! write read cdf



! file load_parameter.f90
! Load the rates of transition form file
!

SUBROUTINE 	load_parameter 

	!	use modules

	USE mod_dynamic_allocation

	USE mod_global

! Variables Declariation 

	IMPLICIT NONE 

	INTEGER:: istat=0, iz,ir,ichk

	CHARACTER*16:: filename,pathname

	filename='t_rate.txt' !default filename of the matrix of transition
	pathname='input/'	  !Default path of the input file

	iunit=2

	CALL openfile (iunit,pathname,filename,filestatus='OLD')

	filename='m_rate.txt'

	CALL openfile (iunit+1,pathname,filename,filestatus='OLD')

!	The unit 10 is used to report input information

!	Body of the subroutine

	READ (iunit,*)

	READ (iunit+1,*)

zoneid:	DO iz=1,n_zone

reaction:	DO ir=1,ntot_reactions

				READ (iunit,*,IOSTAT=istat)					&
&					ichk,									&
&					t_rate(iz,ir)%rate,			    		&
					t_rate(iz,ir)%to_kind
	
				IF (istat.ne.0) CALL readerror (iunit,istat,filename)

				IF (ichk.ne.iz) THEN
					WRITE(10,*) 'WARNING!!! ichk=',ichk,' izone= ',iz
				END IF

			END DO reaction

movement:	DO ir=1,ntot_movements

				READ (iunit+1,*,IOSTAT=istat)				&
&					ichk,									&
&					m_rate(iz,ir)%rate,					&
					m_rate(iz,ir)%to_zone

				IF (istat.ne.0) CALL readerror (iunit+1,istat,filename)

				IF (ichk.ne.iz) THEN
					WRITE(10,*) 'WARNING!!! ichk=',ichk,' izone= ',iz
				END IF

			END DO movement

		END DO zoneid

	CLOSE (iunit)

	CLOSE (iunit+1)


!	Read the non linear parameter from file

	SELECT CASE (c_kindsim)

		CASE ('FRACTURE')
		
			RETURN

		CASE ('LINEAR')

			RETURN

		CASE DEFAULT

			filename='nl_rate.txt'

			iunit=4

			CALL openfile (iunit,pathname,filename,filestatus='OLD')

			READ (iunit,*)

nlinearity:		DO ir=1,ntot_nonlinearity

		zoneid2:	DO iz=1,n_zone

						READ (iunit,*,IOSTAT=istat)				&	
&							ichk ,								&	!Control parameter
&							nl_par(iz,ir)%parameters(1),		&	!First nonlinear parameter
&							nl_par(iz,ir)%parameters(2),		&	!Second nonlinear parameter
&							nl_par(iz,ir)%nlratezero,			&	!Initial value of the nl rate
&							nl_par(iz,ir)%nltype,				&	!kind of nonlinearity
&							nl_par(iz,ir)%k_owner,				&	!particle kind that undergo to this nl rate
&							nl_par(iz,ir)%k_dependent,			&	!particle of which depend the nl rate
&							nl_par(iz,ir)%itrans					!number of the transition that is non linerar

						IF (istat.ne.0) CALL readerror (iunit,istat,filename)
! debug control

						IF (ichk.ne.iz) THEN
	
							WRITE(10,*) 'WARNING!!! ichk=',ichk,' izone= ',iz
	
						END IF
				
					END DO zoneid2

				END DO nlinearity

		CLOSE (unit=iunit)	

	END SELECT
	
END SUBROUTINE 	load_parameter
