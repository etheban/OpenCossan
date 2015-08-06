%% UNIT TEST FOR @STOCHASTICPROCESS
% The class tested here can be found at: http://cossan.co.uk/wiki/index.php/@Function
%%  
% Author: Umar Razzaq
%%
% Individual tests have been split into their own sections to make it easy
% to find out what each test does.
classdef UnitTestStochasticProcess < matlab.unittest.TestCase

    % Contains Properties that will be used in the test block.
    properties 
        Vmat;
        Xcovfun;
        Npoints=7                    
        MmatrixPositive=[4 7  5 ; 1  7 4 ;  7  4   7 ];
    end
    
    % Test class setup: contains coveriance function 
    methods (TestClassSetup)
        function defineMatrices(testCase)
            testCase.Vmat = linspace(0,50,testCase.Npoints); % generates linearly spaced vectors, n number between a and b linspace(a,b,n)
        end
        
        function defineCovarianceFunction(testCase)
            testCase.Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
                  'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
                  'Cinputnames',{'t1','t2'},... % Define the inputs
                  'Sscript', 'sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*abs(Tinput(i).t2-Tinput(i).t1)); end', ...
                  'Coutputnames',{'fcov'}); % Define the outputs
        end
 
    end
%%


    % Methods block: Place individual tests in this block
    methods (Test) % Turn Test attribute on    

        %% test 1: No Mcoord provided
        % Should throw an error if no Mcoord provided
        function testNoMcoordStochasticProcess(testCase)
            testCase.assertError(@()StochasticProcess('Sdescription', 'Description'), 'openCOSSAN:StochasticProcess:StochasticProcess' );
        end
        
        %% test 2: check Sdescription
        % check if Sdescription works
        function testPassSdescriptionStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20);
            testCase.assertEqual(Xsp.Sdescription, 'Description');
        end
        
        %% test 3: incorrect Sdescription
        % check if error thrown if Sdescription is not string
        function testFailSdescriptionStochasticProcess(testCase)
            testCase.assertError(@()StochasticProcess('Sdescription', 45.65), 'openCOSSAN:validateCOSSANInputs');
        end
        
        %% test 4: Normal distribution
        % check if the distribution is normal
        function testPassSdistributionStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdistribution', 'normal', 'Mcoord', testCase.Vmat, 'Vmean', 20);             
            testCase.assertEqual(Xsp.Sdistribution, 'normal');
        end
        
        %% test 5: non-normal distribution
        % only normal distribution is implemented, anything else should
        % give an error
        function testFailSdistributionStochasticProcess(testCase)
            testCase.assertError(@()StochasticProcess('Sdistribution', 'uniform', 'Mcoord', testCase.Vmat, 'Vmean', 20), 'openCOSSAN:StochasticProcess:checkDistribution' );            
        end        
        
        %% test 6: size of Mcoord
        % this counts the size of Mcoord
        function testPassMcoordStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun);   
            testCase.assertSize(Xsp.Mcoord, [1 testCase.Npoints]);
        end
        
        %% test 7: Pass cell to Mcoord
        % should throw an exception if cell is passed to Mcoord
        function testFailMcoordStochasticProcess(testCase)
            try
                StochasticProcess('Sdescription', 'Description', 'Mcoord', cell(4), 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun)
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:validateCOSSANInputs');
            end 
        end
        
        %% test 8: Size of Vmean
        % this checks size of Vmean
        function testPassVmeanStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 23, 'XcovarianceFunction',testCase.Xcovfun);     
            testCase.assertSize(Xsp.Vmean, [1 testCase.Npoints]);
            testCase.assertEqual(Xsp.Vmean(1), 23);
        end
        
        %% test 9: Mean Vector Size
        % Should throw an error if Vmean is greater than Mcoord (sizes
        % should be the same)
        function testFailVmeanStochasticProcess(testCase)

           testCase.assertError(@()StochasticProcess('Mcoord', testCase.Vmat, 'Vmean', 1:testCase.Npoints+1, 'XcovarianceFunction',testCase.Xcovfun),...
               'openCOSSAN:StochasticProcess:StochasticProcess:WrondMeanCoordinate');
        end
        
        %% test 10: Checking XcovarianceFunction name 
        function testPassXcovarianceFunctionStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 23, 'XcovarianceFunction',testCase.Xcovfun);            
            testCase.assertEqual(Xsp.Xcovariancefunction.Sdescription, 'covariance function');
        end
        
        %% test 11: Pass Incorrect XcovarianceFunction
        function testFailXcovarianceFunctionStochasticProcess(testCase) % should fail on initial input, can put in @Parameter object  
            testCase.assertError(@()StochasticProcess('XcovarianceFunction',Parameter), 'openCOSSAN:StochasticProcess:checkCovarianceFunction' );
        end
        %%
        % This should fail as @Parameter object can be passed into input
        
        %% test 12:
        % required if Mcovariance not prodived:
        function testPassCXcovarianceFunction (testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'CXcovarianceFunction', {testCase.Xcovfun}); % but I can also add {testCase.Xcovfun}
            KL_terms(Xsp, 'NKL_terms', 2); 
            testCase.assertEqual(Xsp.Xcovariancefunction.Sscript, 'sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*abs(Tinput(i).t2-Tinput(i).t1)); end');
        end
        
        %% test 13: 
        % what happens when there is two CXcovs in the cell, which one gets
        % used? seems to only use the first one????
        function testPassCXcovariancex2Function (testCase)
           Xcovfun2  = CovarianceFunction('Sdescription','covariance function 2', ...
                                         'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
                                         'Cinputnames',{'t1','t2'},... % Define the inputs
                                         'Sscript', 'sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2.5*exp(-1/b*abs(Tinput(i).t2+Tinput(i).t1)); end', ...
                                         'Coutputnames',{'fcov'}); % Define the outputs'
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'CXcovarianceFunction', {testCase.Xcovfun Xcovfun2}); % but I can also add {testCase.Xcovfun}
            KL_terms(Xsp, 'NKL_terms', 2); 
            testCase.assertEqual(Xsp.Xcovariancefunction.Sscript, 'sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*abs(Tinput(i).t2-Tinput(i).t1)); end');
        end        

        %-------
        %% test 14: correct covariance matrix size
        function testPassMcovarianceStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'Mcovariance', eye(testCase.Npoints)); % change it to 50 to not make it work               
            testCase.assertSize(Xsp.Mcovariance, [testCase.Npoints testCase.Npoints]);
        end
        
        %% test 15: incorrect covariance matrix size
        function testFailMcovarianceStochasticProcess(testCase)
            try
                StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'Mcovariance', eye(50)); 
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:StochasticProcess:StochasticProcess');
                testCase.assertEqual(err.message, 'Size of covariance matrix not correct');
            end
        end  
        
        %% test 16: McovarianceEigenvectors, using lower triangular part of matrix
        % MLBcovariance tested with 2 NKL_terms
        function testPassMLBcovarianceStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'MLBcovariance', tril(eye(testCase.Npoints)) );
            Xsp = KL_terms(Xsp, 'NKL_terms', 2);  
            testCase.assertSize(Xsp.McovarianceEigenvectors, [testCase.Npoints 2]);
        end
        
        %% test 17: using lower part of matrix
        function testPassMLBcovariancex2StochasticProcess(testCase) % k > 1, or upper traingular part of matrix
              % tril = lower triangular part of matrix
                Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat(1:3), 'Vmean', 20, 'MLBcovariance', tril(testCase.MmatrixPositive) );
                testCase.assertClass(KL_terms(Xsp, 'NKL_terms', 2), 'StochasticProcess');

        end          
        
        %% test 18: using upper triangular part of matrix 
        % triu(X) and tril(X, k>1) don't produce any errors, should fail if
        % lower traingular part of matrix is not used
        function testFailMLBcovarianceStochasticProcess(testCase) % k > 1, or upper traingular part of matrix
                Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat(1:3), 'Vmean', 20, 'MLBcovariance', triu(testCase.MmatrixPositive) );
                testCase.assertClass(KL_terms(Xsp, 'NKL_terms', 2), 'StochasticProcess');
        end        

        %% test 19: Incorrect CovarianceFunction size
        % Here the Covariance length is greater than 51 whereas Mcoord is
        % 51
        function testFailMLBcovariancex2StochasticProcess(testCase) % covariancefunc > 51
            try
                StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'MLBcovariance', tril(eye(52)) );
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:StochasticProcess:StochasticProcess');
            end
        end          
        %---------

        %% test 20: check Lhomogeneous is false
        function testPassLhomogeneousStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun, 'Lhomogeneous', false);             
            testCase.assertFalse(Xsp.Lhomogeneous);
        end
        
        %% test 21: Incorrect Lhomogeneous
        % This should throw an error if Lhomogeneous is incorrect.
        function testFailLhomogeneousStochasticProcess(testCase)
            try
                 StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun, 'Lhomogeneous', 'blue');             
            catch err
                 testCase.assertEqual(err.identifier, 'openCOSSAN:validateCOSSANInputs');
            end
        end
        
        %% test 22: testing KL_terms with XcovarianceFunction
        % 
        function testKLterms1StochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun, 'Lhomogeneous', false);                 
            Xsp = KL_terms(Xsp, 'NKL_terms',5);
            testCase.assertSize(Xsp.McovarianceEigenvectors, [testCase.Npoints 5]);
        end        
        
        %% test 23: McovarianceEigenvectors and VcovarianceEigenvalues
        % McovarianceEigenvectors and VcovarianceEigenvalues should be
        % empty at first:
        function testEmptyEigsStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun); 
            testCase.assertEmpty(Xsp.McovarianceEigenvectors);
            testCase.assertEmpty(Xsp.VcovarianceEigenvalues)
        end
        
        %----------------
        
        %% test 24: testing KL_terms with Mcovariance
        function testKLtermsMcovarianceStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'Mcovariance', eye(testCase.Npoints));
            Xsp = KL_terms(Xsp, 'NKL_terms', 5);   % should output testCase.Npoints * 5 matrix
            testCase.assertSize(Xsp.VcovarianceEigenvalues, [5 1]);
            testCase.assertSize(Xsp.McovarianceEigenvectors, [testCase.Npoints 5]); 
        end
        
        %% test 25: 
        % testing sample:
        function testSampleStochasticProcess(testCase)
            Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun, 'Lhomogeneous', false);                 
            Xsp = KL_terms(Xsp, 'NKL_terms',5);
            Xsp.sample('Nsamples', 5);
            
        end


% BOTH OF THESE ERROR OUT, DUE TO SOMETHING WRONG WITH THE CODE
        % using samples before determining KL_terms:
        %% test 26: sample error
        function testSamplesNoKLtermsStochasticProcess(testCase)
            try
                Xsp = StochasticProcess('Sdescription', 'Description', 'Mcoord', testCase.Vmat, 'Vmean', 20, 'XcovarianceFunction',testCase.Xcovfun, 'Lhomogeneous', false);                 
                % Xsp = KL_terms(Xsp, 'NKL_terms',5);
                Xsp.sample('Nsamples', 5);   
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:StochasticProcess:sample');
            end
        end
        
        %--------------------------
        
        %% test 27: check display works
        function testCheckDisplayWorksStochasticProcess(testCase)
            Xsp = StochasticProcess();
            diary('outputWorks');
            Xsp.display();
            diary off 
            content = fileread('outputWorks');
            testCase.assertTrue(all(strfind(content, '* No computed K-L terms ')) );
        end

    end % end methods
end % end class