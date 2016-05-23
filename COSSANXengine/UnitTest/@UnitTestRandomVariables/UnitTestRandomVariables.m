classdef UnitTestRandomVariables <matlab.unittest.TestCase
    % Unit Test for Random Variables
    %   Testing each option of the @RandomVariables command
    
    properties
    end
    
    methods (Test)
        function testRandomVariableNormal(testCase) %tests that a random variable is created with normal distribution and the specified mean and std
            teststdnorm = RandomVariable('Sdistribution','normal','mean',100,'std',15);
            testCase.assertEqual(teststdnorm.mean,100);
            testCase.assertEqual(teststdnorm.std, 15);
        end
        function testRandomVariable(testCase) %test that the command fails when std is not greater than zero
            testCase.verifyError(@()RandomVariable('Sdistribution','normal','mean',15,'std',0),...
                'openCOSSAN:RandomVariable:RandomVariable')
            testCase.verifyError(@()RandomVariable('Sdistribution','normal','mean',15,'std',-2),...
                'openCOSSAN:RandomVariable:RandomVariable')
        end
        function testRandomVariableCov(testCase) %test that the RandomVariable fails when std is not greater than zero 
            testCase.verifyError(@()RandomVariable('Sdistribution','normal','mean',15,'cov',0),...
                'openCOSSAN:RandomVariable:RandomVariable')
            testCase.verifyError(@()RandomVariable('Sdistribution','normal','mean',15,'cov',-2),...
                'openCOSSAN:RandomVariable:RandomVariable')
        end
        function testRandomVariableNormalMeanstdFail(testCase) %test that the command fails as incorrect parameters set 
            testCase.verifyError(@()RandomVariable('Sdistribution','normal','meanstd',100,'std',10),...
                'openCOSSAN:RandomVariable:RandomVariable')
        end
        function testRandomVariableNormalzzdFail(testCase) %test that the command fails as incorrect parameters set 
            testCase.verifyError(@()RandomVariable('Sdistribution','normalzz','mean',100,'std',10),...
                'openCOSSAN:RandomVariable:checkDistribution')
        end
        function testRandomVariableLogNormal(testCase) %test that a random variable is created with lognormal distribution with specified mean and std
            teststdlog = RandomVariable('Sdistribution','lognormal','mean',100,'std',0.10);
            testCase.assertEqual(teststdlog.mean,100);
            testCase.assertEqual(teststdlog.std, 0.10);
        end
        function testRandomVariableLogNormalCpar(testCase) %test that a random variable is created with Log Normal distribution with mu set to 10 and sigma set to 0.1 
            testLogNormalCpar = RandomVariable('Sdistribution','lognormal','Cpar',{'par1',10;'par2',0.1});
            testCase.assertEqual(testLogNormalCpar.Cpar,{'mu',10;'sigma',0.1;[],[];[],[]});
        end
        function testRandomVariableUniform(testCase) %test that a random variable is created with uniform distribution
            testUniform = RandomVariable('Sdistribution','uniform','Cpar',{'par1',1;'par2',2});
            testCase.assertEqual(testUniform.lowerBound,1);
            testCase.assertEqual(testUniform.upperBound,2);
        end
        function testRandomVariableUniformFail(testCase) %test that the command fails if the upper limit is lower than the lower limit
            testCase.verifyError(@()RandomVariable('Sdistribution','uniform','Cpar',{'par1',3;'par2',2}),'openCOSSAN:rv:uniform')
        end
        function testRandomVariableUniformParameterFail(testCase) %test that the command fails as not enough parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','uniform','Cpar',{'par1',3;}),'openCOSSAN:rv:normal')
        end
        function testRandomVariableExponentialCpar(testCase) %test that a random variable is created with exponential distribution with 1/lambda set to 3 and shifting set to 2 
            testExponentialCpar = RandomVariable('Sdistribution','exponential','Cpar',{'par1',3;'par2',2});
            testCase.assertEqual(testExponentialCpar.Cpar,{'1/lambda',3;'shifting',2;[],[];[],[]});
        end
        function testRandomVariableRayleighfail(testCase) %tests that the random variable function with rayleigh distribution fails if parameters are not set correctly
            testCase.verifyError(@()RandomVariable('Sdistribution','rayleigh','mean',100,'cov',0.10),'openCOSSAN:rv:rayleigh');
            testCase.verifyError(@()RandomVariable('Sdistribution','rayleigh','mean','cov',100,0.10),'openCOSSAN:validateCOSSANInputs');
        end
        function testRandomVariableSmall(testCase) %tests the random variable is created with small-i distribution with mean=100 and CoV=0.10
            testSmall = RandomVariable('Sdistribution','small-i','mean',100,'cov',0.10);
            testCase.assertEqual(testSmall.mean,100);
            testCase.assertEqual(testSmall.CoV,0.10);
        end
        function testRandomVariableLarge(testCase) %tests the random variable is created with large-i distribution with mean=100 and CoV=0.10
            testLarge = RandomVariable('Sdistribution','large-i','mean',100,'cov',0.10);
            testCase.assertEqual(testLarge.mean,100);
            testCase.assertEqual(testLarge.CoV,0.10);
        end
        function testRandomVariableGumbel(testCase) %tests the random variable is created with gumbel distribution with mean=100 and CoV=0.10
            testGumble = RandomVariable('Sdistribution','gumbel','mean',100,'cov',0.10);
            testCase.assertEqual(testGumble.mean,100);
            testCase.assertEqual(testGumble.CoV,0.10);
        end
        function testRandomVariableWeibullFail(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','weibull','mean',100,'cov',0.10),'openCOSSAN:RandomVariable:weibull')
        end
        function testRandomVariableWeibull(testCase) %tests the random variable is created with weibull distribution with mean=100 and CoV=0.10
            testWeibull = RandomVariable('Sdistribution','weibull','Cpar',{'par1',3;'par2',2});
            testCase.assertEqual(testWeibull.Cpar,{'a',3;'b',2;[],[];[],[]});
        end
        function testRandomVariableBeta(testCase) %tests the random variable is created with beta distribution with mean=100 and CoV=0.10
            testBeta = RandomVariable('Sdistribution','beta','Cpar',{'par1',3;'par2',2});
            testCase.assertEqual(testBeta.Cpar,{'a',3;'b',2;[],[];[],[]});
        end
        function testRandomVariableGamma(testCase) %tests the random variable is created with gamma distribution with mean=100 and CoV=0.10
            testGamma = RandomVariable('Sdistribution','gamma','Cpar',{'par1',3;'par2',2});
            testCase.assertEqual(testGamma.Cpar,{'k',3;'theta',2;[],[];[],[]});
        end
        function testRandomVariableF(testCase) %tests the random variable is created with f distribution with mean=100 and CoV=0.10
            testF = RandomVariable('Sdistribution','f','Cpar',{'par1',3;'par2',2});
            testCase.assertEqual(testF.Cpar,{'p1',3;'p2',2;[],[];[],[]});
        end
        function testRandomVariableStudent(testCase) %tests the random variable is created with student distribution with mean=100 and CoV=0.10
            testStudent = RandomVariable('Sdistribution','student','Cpar',{'par1',3;'par2',2});
            testCase.assertEqual(testStudent.Cpar,{'nu',3;[],2;[],[];[],[]});
        end
        function testRandomVariableLogistic(testCase) %tests the random variable is created with logistic distribution with mean=100 and CoV=0.10
            testLogistic = RandomVariable('Sdistribution','logistic','Cpar',{'par1',3;'par2',2});
            testCase.assertEqual(testLogistic.Cpar,{'m',3;'s',2;[],[];[],[]});
        end
        function testRandomVariableLogisticFail(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','logistic','Cpar',{'par1',3;}),'openCOSSAN:RandomVariable:logistic')
        end
        function testRandomVariablePoisson(testCase) %tests the random variable is created with poisson distribution with lambda=12
            testPoisson = RandomVariable('Sdistribution','poisson','par1',12);
            testCase.assertEqual(testPoisson.Cpar,{'lambda',12;[],[];[],[];[],[]});
        end
        function testRandomVariablePoissonFail(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','poisson'),'openCOSSAN:RandomVariable:poisson')
        end
        function testRandomVariablePoissonFail2(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','poisson','par1',0),'openCOSSAN:RandomVariable:poisson')
        end
        function testRandomVariablePoissonFail3(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','poisson','mean',0,'std',1),'openCOSSAN:RandomVariable:poisson')
        end
        function testRandomVariableUnidFail(testCase) %test that the command fails as lowerbound must be lower than upperbound
            testCase.verifyError(@()RandomVariable('Sdistribution','unid','lowerbound',3,'upperbound',2),'openCOSSAN:rv:uniformdiscrete')
        end
        function testRandomVariableUnidFail2(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','unid','lowerbound',3),'openCOSSAN:rv:uniformdiscrete')
        end
        function testRandomVariableUnidFail3(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('Sdistribution','unid','mean',5,'std',1),'openCOSSAN:rv:uniformdiscrete')
        end
        function testRandomVariableNoDist(testCase) %test that the command fails as incorrect parameters set
            testCase.verifyError(@()RandomVariable('mean',5,'std',1),'openCOSSAN:RandomVariable:checkDistribution')
        end
        function testRandomVariableNsamples(testCase) %Test that the sample command produces a matrix of 10 random variables
           Xobj = RandomVariable('Sdistribution','poisson','par1',2);
           a = sample(Xobj,'Nsamples',10);
           testCase.assertNotEmpty(a);
           testCase.assertLength(a,10);
        end
        function testRandomVariableGetpdf(testCase) %Test the computation of the emperical pdf for the normal distribution with default bin (100)
            testGetpdf = RandomVariable('Sdistribution','normal','mean',100,'std',0.10);
             a = testGetpdf.getPdf;
             testCase.assertLength(a,101);
        end
        function testRandomVAriableGetpdf2(testCase) %Test the computation of the emperical pdf for the normal distribution with 150 bins
            testGetpdf = RandomVariable('Sdistribution','normal','mean',100,'std',0.10);
             b = testGetpdf.getPdf('Nbins',150);
             testCase.assertLength(b,151);
        end
        function testRandomVariableFittingDistribution(testCase) % Checking that a 1 x100 array of realization data is created
            testFittingDistribution = RandomVariable('Sdistribution','normal','Vdata',rand(100,1));
            c = testFittingDistribution.Vdata;
            testCase.assertLength(c,100)
        end
        function testRandomVariable2DesignVariable(testCase) % checking that the value of the design variable is set to 7 (par1)
        testDesignVariable = RandomVariable('Sdistribution','negativebinomial','par1',7,'par2',0.5);
        d = testDesignVariable.randomVariable2designVariable;
        testCase.assertEqual(d.value,7);
        end
    end
        
end

