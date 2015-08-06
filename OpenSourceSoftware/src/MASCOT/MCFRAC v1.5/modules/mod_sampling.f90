!V.02
!
! Author EDOARDO PATELLI
! Politecnico di Milano
! v.1.0 November-2004
!
MODULE mod_sampling
!
	USE mod_global
	USE mod_dynamic_allocation

	IMPLICIT NONE
	
	CONTAINS
	
!sample form the uniform distribution

		FUNCTION f_dist_uni_meanspan (mean,span) 

		! Variables

			REAL f_dist_uni_meanspan,mean, span

		! Body of the function
			
			f_dist_uni_meanspan=RAN(ISEED)*(2*span)+mean-span

		END FUNCTION f_dist_uni_meanspan

		FUNCTION f_dist_uni_startstop (xstart,xstop) 

		! Variables

			REAL f_dist_uni_startstop,xstart,xstop

		! Body of the function
			
			f_dist_uni_startstop=RAN(ISEED)*(xstop-xstart)+xstart

		END FUNCTION f_dist_uni_startstop
			

!sample form the exponential distribution

		FUNCTION f_dist_exp (lambda) 

		! Variables

			REAL f_dist_exp, lambda

		! Body of the function
			
			f_dist_exp=-LOG(1-RAN(ISEED))/lambda

		END FUNCTION f_dist_exp

!sample form the exponential distribution (passing the inverse of lambda)

		FUNCTION f_dist_exp_inv (invlambda) 

		! Variables

			REAL f_dist_exp_inv, invlambda

		! Body of the function
			
			f_dist_exp_inv=-LOG(1-RAN(ISEED))*invlambda

		END FUNCTION f_dist_exp_inv
		
!	sample form the proportional distribution 
											
		FUNCTION f_dist_pro (coef,maxvalue) 

		! Variables

			REAL f_dist_pro , coef,maxvalue
	
		! Body of the function

		f_dist_pro=maxvalue*(RAN(ISEED)**( 1/(coef+1) ) )

		END FUNCTION f_dist_pro

!	sample form the gaussian distribution with mean values "mval"
!	and standard deviation "std"
											
		FUNCTION f_dist_gauss (mval,std) 

		! Variables

			REAL f_dist_gauss, mval,std

			INTEGER I
					
		! Body of the function
			
			f_dist_gauss=-6

			DO I=1,12
				
				f_dist_gauss=f_dist_gauss+RAN(ISEED)
		
			END DO

			f_dist_gauss=mval+std*f_dist_gauss

		END FUNCTION f_dist_gauss

!
!	sample lognormal distribution from the 5th percentile and
!	the 95-th percentile
!

		FUNCTION f_dist_logn_0595 (p05,p95) 

		! Variables

			REAL f_dist_logn_0595,p05,p95,mval,std

		! Body of the function

		! Compute the mean value of the log-normal distribution
			
			mval=LOG(p95*p05)/2.

		! Compute the standard deviation of the log-normal distribution

			std=LOG(p95/p05)/3.29

		! Compute the value of the function

			f_dist_logn_0595=EXP(f_dist_gauss(mval,std))

		END FUNCTION f_dist_logn_0595

!
!	sample lognormal distribution from the mean and standar deviation
!

		FUNCTION f_dist_logn (mval,std) 

		! Variables

			REAL f_dist_logn,mval,std

		! Body of the function

		! Compute the value of the function

			f_dist_logn=EXP(f_dist_gauss(mval,std))

		END FUNCTION f_dist_logn
	

END MODULE mod_sampling

