%% UNIT TEST FOR @MONTECARLO
% The class tested here is MonteCarlo 
%
% Written by Umar Razzaq, 2014 University of Liverpool
% Revised by Edoardo Patelli
%
%%
% Individual tests have been split into their own sections to make it easy
% to find out what each test does.

%% xUnit sub-class defintion
% This sub-class inherits from matlab.unittest.TestCase 
classdef UnitTestMonteCarlo < matlab.unittest.TestCase
%%

    % This block contains all the properties that are used in the Test.
    properties   
        Xin;        % Store Input Model
        Xmdl;       % Store Model
        Xpm;        % Store Pobabilisitic model
    end
   
    
    %% Class Fixture
    % This sets up the problem from the tutorial
    methods (TestClassSetup)
        function defineModel(testCase)
            % Define 2 random variable
            RV1=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);
            RV2=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);
            
            % Define the RVset
            Xrvs1=RandomVariableSet('Cmembers',{'RV1', 'RV2'},'CXrv',{RV1 RV2});
            
            % Define Input object
            testCase.Xin = Input('Sdescription','Input satellite_inp');
            Xthreshold=Parameter('value',1);
            Xadditionalparameter=Parameter('Vvalue',rand(100,1));
            testCase.Xin = add(testCase.Xin,Xrvs1);
            testCase.Xin = add(testCase.Xin,Xthreshold);
            testCase.Xin = add(testCase.Xin,Xadditionalparameter); %#ok<*PROP>
           
            Xm=Mio(         'Sdescription', 'Performance function', ...
                            'Sscript','for j=1:length(Tinput), Toutput(j).out1=sqrt(Tinput(j).RV1^2+Tinput(j).RV2^2); end', ...
                            'Liostructure',true,...
                            'Coutputnames',{'out1'},...
                            'Cinputnames',{'RV1','RV2'},...
                            'Lfunction',false); % This flag specify if the .m file is a script or a function.


            % Construct the Evaluator
            Xeval = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');
                                    
            % Define a model:
            testCase.Xmdl = Model('Xevaluator', Xeval, 'Xinput', testCase.Xin);          

            % construct performance function
            Xperffun = PerformanceFunction('Sdemand', 'out1', 'Scapacity', 'Xthreshold', 'SoutputName', 'Vg');
            
            % construct ProbabilisticModel Object
            testCase.Xpm = ProbabilisticModel('Xmodel', testCase.Xmdl, 'XPerformanceFunction', Xperffun);
           
        end % end defineModel 

        
    end % end TestClassSetup
    
    %% 
    
    % Methods Block: Place individual tests in this block
    methods (Test)
        
        %% test 1: check empty Monte Carlo object:
        % Should return expected properties of empty MonteCarlo object
        function testEmptyObjectMonteCarlo(testCase)
            % output = 'This checks the expected properties of an empty MonteCarlo object'
            expProp = {'Nsimxbatch';
                       'Nlastbatch';
                       'SbatchName';
                       'Sdescription';
                       'Lverbose';
                       'CoV';
                       'timeout';
                       'Nsamples';
                       'confLevel';
                       'Nbatches';
                       'Lintermediateresults';
                       'XrandomStream';
                       'SbatchFolder'};
             
             Xmc = MonteCarlo();
             msg = 'Checking actual and expected properties of an empty Monte Carlo Object, not as expected';
             testCase.assertEqual(properties(Xmc), expProp, msg);      
        end
       %%
       % 
       %
       % Status: Should Pass        
        
        %% test 2: check Nsamples, 10 of them:
        % checks Nsamples property  
        function tenNsamplesMonteCarloObject(testCase)
            Nsamples=randi(50,1);
            Xmc = MonteCarlo('Nsamples', Nsamples);
            testCase.assertEqual(Xmc.Nsamples, Nsamples, strcat('Namples is not equal to',sprintf(' %i',Nsamples)));
        end        
       % Status: Should Pass 
        
        %% test 3: check class type 
        % this test checks that the output is of class MonteCarlo
        function monteCarloClass(testCase)
           Xmc = MonteCarlo;
           testCase.assertClass(Xmc, 'MonteCarlo');
        end
       % Status: Should Pass 
       
       
        %% test 4: check Nsample type, empty object:
        % checks Nsamples property of an empty object
        function zeroNsamplesMonteCarloObject(testCase)
            Xmc = MonteCarlo;
            testCase.assertEqual(Xmc.Nsamples, 1, 'Nsamples is not equal to 1');
        end
       % Status: Should Pass         
        
        %% test 5: test that checks apply method
        % this test checks that when apply method is used the output object
        % should be a SimulationData class.
        function applyMethodOutputsSimulationData(testCase)
           Nsamples=randi(50,1);
           Xmc = MonteCarlo('Sdescription', 'Unit Test', 'Nsamples', Nsamples, 'Nbatches', 1);
           Xout = Xmc.apply(testCase.Xmdl);
           testCase.assertClass(Xout, 'SimulationData', 'Output class should be SimulationData');
           testCase.assertEqual(Xout.Nsamples, Nsamples, ['Nsamples in the SimulationData shoul be equal to ',sprintf(' %i',Nsamples)]);
        end
       % Status: Should Pass 
       
        %% test 6: check sample method
        % checks that sample method outputs a Sample class
        function sampleMethodOutputsSampleData(testCase)
           Xmc = MonteCarlo('Sdescription', 'Unit Test', 'Nbatches', 1);
           Nsamples=randi(50,1);
           Xsample = Xmc.sample('Nsamples', Nsamples, 'Xinput', testCase.Xin);
           testCase.assertClass(Xsample, 'Samples');
           testCase.assertEqual(Xsample.Nsamples,Nsamples,['Nsamples stored in the Sample object should be equal to %i', sprintf(' %i',Nsamples)]);
        end
       % Status: Should Pass 
       
        %% test 7: checks that timeout works
        % This works by providing plentiful time (999 seconds) to compute 1
        % sample and 1 batch. The timeout should not be exceeded. This
        % timeout should not be exceeded on any modern computer
        function timeoutShouldNotTerminate(testCase)
           Xmc = MonteCarlo('Sdescription', 'Unit Test', 'timeout', 999, 'Nsamples', 1, 'Nbatches', 1);
           tic; % starts the timer
           Xout=Xmc.apply(testCase.Xmdl);
           time = toc; % stop the timer
           testCase.assertLessThanOrEqual(time, Xmc.timeout);
           testCase.assertSubstring(Xout.SexitFlag, 'Maximum no. of samples reached');
        end
       % Status: Should Pass         
        
        %% test 8: check timeout works 2
        % This works the same as test 7 but this checks that the maximum
        % execution time is reached by providing a very short timeout.
        function timeoutShouldTerminate(testCase)
           Xmc = MonteCarlo('Sdescription', 'Unit Test', 'timeout', 0.1, 'Nsamples', 99999999, 'Nbatches', 99999999);
           tic; % starts the timer
           Xout=Xmc.apply(testCase.Xmdl);
           time = toc; % stop the timer
           testCase.assertGreaterThanOrEqual(time, Xmc.timeout);  
           testCase.assertSubstring(Xout.SexitFlag, 'Maximum execution time reached');
        end
       %%
       % 
       %ans
       % Status: Should Pass  
       
        %% test 9: check SexitFlag works, when it does timeout:
        % This test checks that if the time taken to execute the apply
        % method, does the SexitFlag contain the correct message.
        function timeoutFlag(testCase)
           Xmc = MonteCarlo('Sdescription', 'Unit Test', 'timeout', 2, 'Nsamples', 999999, 'Nbatches', 999999);
           tic; % starts the timer
           Xout = Xmc.apply(testCase.Xmdl);
           %time = toc; % stop the timer
           testCase.assertSubstring(Xout.SexitFlag, 'Maximum execution time reached');
        end
       % Status: Should Pass  
       
        %% test 10: test incorrect number of batches should throw an error
        % This checks that the number of batches (10) cannot be greater
        % than the number of samples (5)
        function numberOfBatches(testCase)
             try
                 MonteCarlo('Nsamples', 5, 'Nbatches', 10, 'timeout', 5000);
             catch err 
                 testCase.assertEqual('openCOSSAN:simulations:MonteCarlo', err.identifier); 
             end
        end
       % Status: Should Pass as the execution should be thrown
        
        %% test 11: number of Xout samples should be one
        % This test checks that if 10 samples and 10 batches are provided,
        % the number ouput of SimulationData should be 1 samples.
        function xOutSamples(testCase)
            Xmc = MonteCarlo('Nsamples', 10, 'Nbatches', 10, 'timeout', 5000);
            Xout = Xmc.apply(testCase.Xmdl);
            testCase.assertEqual(Xout.Nsamples, 1);
        end
       % Status: Should Pass        
        
       %% test 12: test NseedRandomNumberGenerator for two objects (fails)
          % This test checks that the same stream of random numbers is generated for both Simulation Data 
          % objects when the same random number seed is used.
        function nSeedRandomNumberGenerator(testCase)
            Xmc = MonteCarlo('Nsamples', 10, 'Nbatches', 1, 'NseedRandomNumberGenerator', 0);
            Xout1 = Xmc.apply(testCase.Xmdl);
            Xmc = MonteCarlo('Nsamples', 10, 'Nbatches', 1, 'NseedRandomNumberGenerator', 0);
            Xout2 = Xmc.apply(testCase.Xmdl); 
            testCase.assertEqual(Xout1.getValues('Sname','out1'), Xout2.getValues('Sname','out1'));
        end
       %%
       % 
       %
       % Status: Throws an error:  
       %     Error using RandStream.getDefaultStream (line 457)
       %     The RandStream.getDefaultStream static method has been removed.  Use RandStream.getGlobalStream instead.
       %     
       %     Error in Simulations/initializeUserDefinedRandomNumberGenerator (line 32)
       %     XRandomNumberGenerator   = RandStream.getDefaultStream;       
        

        %% test 13: RandomNumberGenerator 1  
        % This checks if an exception is thrown when an incorrect field
        % name (RandomNumberGenerator) is used.
        function testRandomNumberGenerator(testCase)
            try
                Xmc = MonteCarlo('Sdescription', 'Unit Test #9', 'Nsamples', 10, 'Nbatches', 1, 'RandomNumberGenerator', 0);
                Xmc.apply(testCase.Xmdl);
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:simulations:MonteCarlo'); 
            end
        end
       % Status: Should Pass as the exception should get thrown   
        
       %% DO not test RandomNumberGenerator, it needs to be re
%        % test 14: test XRandomNumberGenerator accepts new RandStream object 
%        This test should pass as it does not accept a RandStream object
%        without throwing an error like test 12 (above)
%         function testXrandomNumberGenerator(testCase)
%             s = RandStream('mlfg6331_64','Seed',1); 
%             MonteCarlo('Sdescription', 'Unit Test #9', 'Nsamples', 10, 'Nbatches', 1, 'XRandomNumberGenerator', s)
%             testCase.assumeError(@()Xmc.apply(testCase.Xmdl), 'MATLAB:RandStream:GetDefaultStream')
%         end
       %%
       % 
       %
       % Status: Expected to pass as a error should be thrown
       % ERROR: 
       %
       %
        %       Error using RandStream.getDefaultStream (line
        %       457)
        %   The RandStream.getDefaultStream static method
        %   has been removed.  Use
        %   RandStream.getGlobalStream instead.
        % 
        %   Error in
        %   Simulations/initializeUserDefinedRandomNumberGenerator
        %   (line 32)
        %   XRandomNumberGenerator   =
        %   RandStream.getDefaultStream;
        % 
        %   Error in MonteCarlo/apply (line 40)
        %     XRandomNumberGenerator = ...
        %   test 15: criteria cov and method pf (test 12)
        
        %% test 15: check FailureProbability Input
        % checks if exception is thrown if a class of FailureProbability is
        % input
        function failPerformanceFunction(testCase)
            try
                Xmc=MonteCarlo('Sdescription', 'Unit Test', 'cov',0.5, 'Nsamples',10, 'Nbatches',1);    
                Xmc.computeFailureProbability(testCase.Xmdl);
            catch err
                testCase.assertEqual( 'openCOSSAN:Simulations:checkInputs:ModelNotAllowed', err.identifier);
            end
        end
       % Status: Should Pass as the exception should get thrown   
       
       %% test 16: run computeFailureProbability 
        % checks if exception is thrown if a class of FailureProbability is
        % input
        function runComputeFailureProbability(testCase)
            try
                Xmc=MonteCarlo('Sdescription', 'Unit Test', 'cov',0.5, 'Nsamples',10, 'Nbatches',1);    
                Xmc.computeFailureProbability(testCase.Xpm);
            catch err
                testCase.assertEqual('openCOSSAN:Simulations:checkInputs', err.identifier);
            end
        end
       % Status: Should Pass as the exception should get thrown   
        
    end   
end