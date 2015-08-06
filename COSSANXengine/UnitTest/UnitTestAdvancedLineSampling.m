%% UNIT TEST FOR @ADVANCEDLINESAMPLING
% The class tested here can be found at: http://cossan.co.uk/wiki/index.php/@AdvancedLineSampling
%%  
% Author: Umar Razzaq
% Revised by Edoardo Patelli
%%
% Individual tests have been split into their own sections to make it easy
% to find out what each test does.

classdef UnitTestAdvancedLineSampling < matlab.unittest.TestCase
%%
    
    % Contains model properties used in test below
    properties
        X1;
        X2;
        Xrvs;
        a;
        maxDistance;
        Xmio;
        Xevaluator;
        Xmodel;
        Xperformance;
        XprobModel;
        Xlsfd;
        Xgrad;
        ValphaGRA;
        Xinde;
        ValphaLSM;
        Xinput;
        Xo;
        Xg;
    end
    
    %% Problem Definition, this uses a class setup fixture to all access to these properties in all functions of the class
    methods (TestClassSetup)
        
        % this defines the problem found in the tutorial for example 1 by
        % Marco
        function defineTutorialModel(testCase)
            % two random variables:
            testCase.X1=RandomVariable('Sdistribution','normal', 'mean',3,'std',1);
            testCase.X2=RandomVariable('Sdistribution','normal', 'mean',2,'std',1);

            % define random variable set:
            testCase.Xrvs=RandomVariableSet('CSmembers',{'X1', 'X2'},...
                                   'CXrandomVariables',{testCase.X1, testCase.X2});

            % parameter for defining capacity of model:
            testCase.a = [9 9.5 9.7 10 10.3 10.9]; 
            testCase.maxDistance = Parameter('value', 10);

            % input object, put in random variable set and parameter:
            testCase.Xinput = Input('Sdescription','Input Object', ...
                           'CXmembers',{testCase.Xrvs testCase.maxDistance},...
                           'CSmembers',{'Xrvs' 'maxDistance'});
                       
            % construct mio object:
            testCase.Xmio=Mio('Sdescription', 'Matlab I-O for the demand of the system',...
                     'Sscript',['for j=1:length(Tinput),',...
                     'Toutput(j).demand=sqrt(Tinput(j).X1^2+Tinput(j).X2^2); end'],...
                     'Liostructure',true,...
                     'Coutputnames',{'distance'},...
                     'Cinputnames',{'X1' 'X2'},...
                     'Lfunction',false);
                 
            % construct evaluator:
            testCase.Xevaluator = Evaluator('Xmio',testCase.Xmio,'Sdescription','Evaluate demand of the system');
            
            % define physical model, the distance between the two random
            % variables:
            testCase.Xmodel=Model('Xevaluator',testCase.Xevaluator,'Xinput',testCase.Xinput);
            
            % test model:
            testCase.Xinput = sample(testCase.Xinput,'Nsamples',10);
            % Check the Model object
            testCase.Xo = apply(testCase.Xmodel,testCase.Xinput);
            
            % performance function:
            testCase.Xperformance=PerformanceFunction('Scapacity','maxDistance',...
                                             'Sdemand','demand','SoutputName','Vg');
            
            % probabilistic model:
            testCase.XprobModel=ProbabilisticModel('Xmodel',testCase.Xmodel,...
                                          'XperformanceFunction',testCase.Xperformance);
                                      
            % computer important direction:
            testCase.Xlsfd=LocalSensitivityFiniteDifference('Xtarget',testCase.XprobModel, ...
                                                   'Coutputnames',{'Vg'});
            % Compute the Gradient
            Xgrad = testCase.Xlsfd.computeGradient; %#ok<*PROP>
            testCase.Xg = Xgrad;
            testCase.ValphaGRA= -Xgrad.Valpha;
            
            % Compute the Indices
            Xinde = testCase.Xlsfd.computeIndices;
            testCase.ValphaLSM= -Xinde.Valpha; 
            
            testCase.Xinde = Xinde;
        end
        
    end
%%    
   % Methods block: Place individual tests in this block
   methods (Test) % Turn Test attribute on
        
       
        %% test 1: testing empty constructor properties:
        function testConstructorAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling();
            
            expProp = { 'Valpha';
                        'CalphaNames';
                        'Nlinexbatch';
                        'Nlinelastbatch';
                        'Nvars';
                        'Nlines';
                        'Ncfine';
                        'acceptableError';
                        'tolerance';
                        'NmaxPoints';
                        'minStep';
                        'maxStep';
                        'reliabilityIndex';
                        'XlineSamplingData';
                        'CMstatePoints';
                        'LdoNotUpdateDirection';
                        'Nsimxbatch';
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
                    
             testCase.assertEqual(properties(Xals), expProp);
        end
        
        
        %% test 2: no important direction defined
        % no previous analysis was done so Valpha should be empty:
        function testNoImportantDirectionAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling();
            testCase.assertEmpty(Xals.Valpha);
        end
        
        %% test 3: number of batches
        % this test that the number of batches cannot be greater than number of lines:
        function testErrorBatchesGreaterThanLinesAdvancedLineSampling(testCase)
            testCase.assertError( @()AdvancedLineSampling('Nbatches', 7, 'Nlines', 6), 'openCOSSAN:simulations:AdvancedLineSampling' );
        end
        
        %% test 4: lines per batch
        % this chceks this calculation: floor(Xobj.Nlines/Xobj.Nbatches), 
        function testNlinexbatchAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Nbatches', 6, 'Nlines', 6);
            testCase.assertEqual(Xals.Nlinexbatch,1);
        end
        
        %% test 4: test Nlinelastbatch
        function testNlinelastbatchAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Nbatches', 6, 'Nlines', 12);
            testCase.assertEqual(Xals.Nlinelastbatch, 2);
        end
        
        %% test 5: o NsamplesUD
        % if NsamplesUD doesnt exist Nsamples=Xobj.Nlines*Xobj.NmaxPoints
        function testNsamplesAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Nbatches', 6, 'Nlines', 12, 'NmaxPoints', 4);
            testCase.assertEqual(Xals.Nsamples, 48);
        end
                
        %% test 6: check invalid input
        % Xbatches is invalid input
        function testInvalidInputAdvancedLineSampling(testCase)
            testCase.assertError( @()AdvancedLineSampling('Xbatches', 6, 'Nlines', 12), 'openCOSSAN:validateCOSSANInputs' )
        end
        
        %% test 7: check Xgradient is gradient object
        function testXgradientNotObjectAdvancedLineSampling(testCase)
            try
                notGrad = LineSampling();
                AdvancedLineSampling('Nbatches', 6, 'Nlines', 12, 'Xgradient', notGrad);
            catch err
                testCase.assertEqual(err.identifier, 'openCOSSAN:AdvancedLineSampling:wrongGradientType');
            end
        end
               
        %% test 8: check Nvars = length(Xobj.Valpha);
        function testNvarsAdvancedLineSampling (testCase)
            Xals = AdvancedLineSampling('Nlines',30,'Vdirection',testCase.ValphaGRA);
            testCase.assertEqual(Xals.Nvars, 2)
        end
        
        
        %% test 9: test NseedRandomNumberGenerator
        function testNseedRandomNumberGeneratorAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('NseedRandomNumberGenerator', 834457);
            testCase.assertEqual(Xals.XrandomStream.Seed, uint32(834457));
        end
        
        %% test 10: test XrandomNumberGenerator
        function testXrandomNumberGeneratorAdvancedLineSampling(testCase)
            s = RandStream('mlfg6331_64', 'seed', 789789);
            Xals = AdvancedLineSampling('XrandomNumberGenerator', s);
            testCase.assertEqual(isequaln(Xals.XrandomStream, s),true);
        end
        
        %% test 11: not a randstream object
        % should give a warning if RandStream object is not passed in
        function testErrorNotRandStreamAdvancedLineSampling(testCase)
            s = Parameter();
            testCase.verifyWarning(@()AdvancedLineSampling('XrandomNumberGenerator', s)  ,'openCOSSAN:AdvancedLineSampling')
        end
        
        %% testing .sample method
        
        %% test 12: sample number
        % generate samples, relies internally on Samples object:
        function testSamplesAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Nlines', 3, 'Vdirection', testCase.ValphaLSM);
            Xsample = Xals.sample('Xinput', testCase.Xinput);
            testCase.assertEqual(Xsample.Nsamples, 3);
        end
        
        %% test 13: MsamplesStandardNormalSpace size
        function testMsamplesStandardNormalSpace(testCase)
            import matlab.unittest.constraints.HasSize;
            Xals = AdvancedLineSampling('Nlines', 11, 'Vdirection', testCase.ValphaLSM);
            Xsample = Xals.sample('Xinput', testCase.Xinput);
            testCase.assertThat(Xsample.MsamplesStandardNormalSpace, HasSize([11 2]));
        end
        
        %% test 14: samples generated should be different each time:
        function testCompareTwoSamples(testCase)
            Xals1 = AdvancedLineSampling('Nlines', 11, 'Vdirection', testCase.ValphaLSM);
            Xsample1 = Xals1.sample('Xinput', testCase.Xinput); 
            
            Xals2 = AdvancedLineSampling('Nlines', 11, 'Vdirection', testCase.ValphaLSM);
            Xsample2 = Xals2.sample('Xinput', testCase.Xinput); 
            
            testCase.verifyFalse(isequal(Xsample1.MsamplesPhysicalSpace, Xsample2.MsamplesPhysicalSpace))  
        end
        
        %% changing algo parameters
        
        %% test 15: should 0 max points be allowed?
        function testNmaxPoints(testCase)
            Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM);
            Xals.NmaxPoints = 0;   
            
            % [Xpf Xout] = testCase.XprobModel.computeFailureProbability(Xals)
        end
        
        %% test 16: large Nmax points
        % should 0 max points be allowed? CRASHES,DO NOT RUN the commented
        % out bit
        function testNmaxPointsx2(testCase)
            Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM);
            Xals.NmaxPoints = 9999999;   
            
            % [Xpf Xout] = testCase.XprobModel.computeFailureProbability(Xals)
        end
        
        %% test 17: zero min points
        % should min number of points be zero: it errors out here, only
        % when you run it without the try catch, but its not this error
        function minStep (testCase)

                Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM);
                Xals.NmaxPoints = 0;  
                Xals.minStep = 0;

                testCase.assertError(@()Xals.computeFailureProbability(testCase.XprobModel), ...
                    'openCOSSAN:AdvancedLineSampling:exploitLine:maxPoint')

        end
%%        
%         function zeroAcceptanceError(testCase) % works with performance function
%             Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM);
%             Xals.acceptableError = 1e-9999999999999; 
%             Xlsfd = LocalSensitivityFiniteDifference()
%             [Xpf Xout] = testCase.XprobModel.computeFailureProbability(Xals);
%         end
        
%         function zeroTolerance(testCase)
%             Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM);
%             Xals.tolerance = 1e-999999999; % tolerance on newtons iteration
%         end
        
        
%         % function test Ldonotupdatedirection
%         function testLdonotupdatedirection (testCase)
%             Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM, 'Ldonotupdatedirection', true);
%         end
        

        %% 
        %% test 18: testing apply method (apply a model)
        
        function testApplyMethod(testCase) % this should not error but it does
            Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM);
            Xals.apply(testCase.Xmodel);
        end
        %%
        % This causes an error
%Error using AdvancedLineSampling/sample (line 22)
% Input parameter Nlines not allowed
% 
% Error in AdvancedLineSampling/apply (line 69)
%Xs=Xobj.sample('Nlines',Nlines,'Xinput',Xinput);
            
                   
        %% test 19: sbatchfolder
        function testSbatchFolder (testCase)
            Xals = AdvancedLineSampling('Vdirection', testCase.ValphaLSM, 'SbatchFolder', 'OutResults','Nlines',2);
            [~, ~] = testCase.XprobModel.computeFailureProbability(Xals);
            
            % Check if the results have been saved 
            testCase.assertEqual(exist(fullfile(OpenCossan.getCossanWorkingPath,Xals.SbatchFolder,[Xals.SbatchName '.mat']),'file'), 2);
        end
        
        %%
        
%   the following tests are documented in my report,this checks an extract of Advanced Line Sampling which checks important direction
        
        %% test 20: Path 1
        % (Path 1): Xgradient object not passed, should not set Valpha
        function testPath1AdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling();
            testCase.assertEmpty(Xals.Valpha);
        end
        
        %% test 21: Path 2
        % Path 2: Pass in Xgradient and set Vvalue :
        function testPath2AdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Xgradient', testCase.Xg);
            % testing the class:
            expVal = -testCase.Xg.Valpha;
            testCase.assertEqual(Xals.Valpha, expVal);
        end       
        
        %% test 21: Path 5
        % Path 5: An Xgradient object is provided as input but the Valpha property is not empty
        function testPath5AdvancedLineSampling(testCase)
            try
                AdvancedLineSampling('Xgradient', testCase.Xg, 'Vdirection', testCase.ValphaLSM)
            catch err
                testCase.assertEqual(err.message, 'At least a Gradient object is required to define the important direction')
                testCase.assertEqual(err.identifier, 'openCOSSAN:AdvancedLineSampling');
            end
        end
        
        %% test 22: check display works
        function testCheckDisplayWorksAdvanedLineSampling(testCase)
            Xals = AdvancedLineSampling();
            diary('outputWorks');
            Xals.display();
            diary off 
            content = fileread('outputWorks');
            testCase.assertTrue(all(strfind(content, 'Important direction: NOT DEFINED')) );
        end
        
        %%
        % test computeFailureProbability
        %%
        
        %% test 23: check sexitflag reached
        function testComputeFailureProbabilityAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Nlines',5,'Vdirection',testCase.ValphaLSM);
            [Xpf,~]=testCase.XprobModel.computeFailureProbability(Xals);
            testCase.assertEqual(Xpf.SexitFlag, 'Maximum no. of lines reached. Lines computed 5; Max Lines : 5');
        end
        
        %% test 24: check xout takes cNames from problem:
        function testComputeFailureProbabilityx2AdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Nlines',7,'Vdirection',testCase.ValphaLSM);
            [~,Xout]=testCase.XprobModel.computeFailureProbability(Xals);
            testCase.assertEqual(Xout.Cnames, {'X1'; 'X2'; 'maxDistance'; 'distance'; 'demand'; 'Vg'});
        end        
        
        %% test 25:  is there a pfhat value? should be around 0.01
        function testComputeFailureProbabilityx3AdvancedLineSampling(testCase)
            OpenCossan.resetRandomNumberGenerator(100);
            Xals = AdvancedLineSampling('Nlines',10,'Vdirection',testCase.ValphaLSM);
            [Xpf,~]=testCase.XprobModel.computeFailureProbability(Xals);
            testCase.assertLessThan((Xpf.pfhat-1.644e-10),1e-4);
        end           

        %% test 26: test apply
        % Xo=Xsim.apply(Xmodel)      
        function testApplyMethodAdvancedLineSampling(testCase)
            Xals = AdvancedLineSampling('Vdirection',testCase.ValphaLSM);
            Xals.apply(testCase.Xmodel)
        end
% LineSampling: Check inputs
% LineSampling: Start analysis
% Error using AdvancedLineSampling/sample (line 22)
% Input parameter Nlines not allowed
% 
% Error in AdvancedLineSampling/apply (line 69)
% Xs=Xobj.sample('Nlines',Nlines,'Xinput',Xinput); 
        
        
    end 
end



