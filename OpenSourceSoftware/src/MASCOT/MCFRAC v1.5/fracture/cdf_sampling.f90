! Subroutine Sampling from cdf

		SUBROUTINE cdf_sampling (value,i_ch,kcdf)

			USE mod_global
			USE mod_dynamic_allocation

		! Variables

			REAL,INTENT(OUT):: value
			REAL:: interpolate
			REAL:: sample,span

			INTEGER,INTENT(OUT):: i_ch

			CHARACTER (LEN=3),INTENT(IN):: kcdf


			! Body of the function

    		! Compute the value of the function

			sample=RAN(ISEED)
			
			! find the sampled channal 
			i_ch=1
			DO WHILE (sample.GE.p_cdf(i_ch))
				i_ch=i_ch+1
			END DO

			IF (i_ch.GT.SIZE(p_cdf)) THEN			!Debug
				write (*,*) 'cdf sampling warning'	!Debug
			END IF									!Debug

			SELECT CASE (kcdf)

				CASE ('UNI','uni') !the bins are spaced uniformly
					span=(p_bin(2)-p_bin(1))

					IF (i_ch.EQ.1) THEN
						interpolate=(span)/(p_cdf(i_ch))
						value=(p_bin(i_ch)-span/2)+interpolate*(sample)
					ELSEIF (i_ch.EQ.SIZE(p_bin)) THEN
						sample=RAN(ISEED)*span
						value=(p_bin(i_ch)+p_bin(i_ch-1))/2+sample		
					ELSE 
						interpolate=(span)/(p_cdf(i_ch)-p_cdf(i_ch-1))
						value=(p_bin(i_ch)+p_bin(i_ch-1))/2+interpolate*(sample-p_cdf(i_ch-1))			
					END IF

				CASE ('FIX','fix') ! no interpolation -> discrete cdf
					value=p_bin(i_ch)
						
				CASE ('USR','usr') !each bin has different span

					IF (i_ch.EQ.1) THEN
						span=(p_bin(i_ch+1)-p_bin(i_ch))
						interpolate=(span)/(p_cdf(i_ch))
						value=(p_bin(i_ch)-span/2)+interpolate*(sample)
					ELSE 
					    span=p_bin(i_ch)-p_bin(i_ch-1)
						interpolate=(span)/(p_cdf(i_ch)-p_cdf(i_ch-1))
						value=(p_bin(i_ch)+p_bin(i_ch-1))/2+interpolate*(sample-p_cdf(i_ch-1))			
					END IF

				CASE DEFAULT
					WRITE (*,*) 'Kind of cdf not yet implemented (',kcdf,')'
			END SELECT
			
		IF (p_cdf(i_ch).eq.0) THEN
			WRITE (*,*) 'Warning: cdf sampling'
		END IF


		 IF (l_writelog) THEN
			WRITE (99,*) 'cdf_sampling'
		END IF

		END SUBROUTINE cdf_sampling
