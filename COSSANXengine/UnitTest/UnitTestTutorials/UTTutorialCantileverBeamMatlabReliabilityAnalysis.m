classdef UTTutorialCantileverBeamMatlabReliabilityAnalysis < matlab.unittest.TestCase                          %Enter Script name after UT
    
    properties
        %Enter Test Number
        TestNum     = 10;       
        % Enter the names of any scripts that need to be run  prior to
        % performing the test into a cell array. 
        PreTest     = {'TutorialCantileverBeamMatlab'}; 
        % Enter the name of the script to be tested.
        Name        = 'TutorialCantileverBeamMatlabReliabilityAnalysis';   
        % Enter Variable Names into cell array created
        VName       = {'XfailireProbMC.pfhat' 'XfailireProbLHS.pfhat'...
                       'XfailireProbLS.pfhat' 'XfailireProbLS2.pfhat'};    
        % Enter the expected results into cell array
        Expected    = {0.0738 0.083 0.06683 0.07267};      
        % Enter the given tolerances into cell array
        Tolerance   = {eps eps 0.000001 0.0001};    
        %Enter the number of tests being performed (no. of variables)
        NumOfTests  = 4;                                        
        
        Actual      = [];
        ErrorPC     = [];
        ErrorPCT    = [];
        SLocation   = pwd;
    end
    
    methods(TestMethodSetup)
        function Initiate(TestCase)
            
            for i = 1:length(TestCase.PreTest)
                if isempty(TestCase.PreTest{i}) == 0;
                    eval(TestCase.PreTest{i});
                end
            end
            
            eval(TestCase.Name);
            
            for i = 1:TestCase.NumOfTests
                TestCase.Actual{i} = eval(TestCase.VName{i});
            end
        end
    end

    methods (TestMethodTeardown)
        function SaveData(TestCase)
            if exist(fullfile(TestCase.SLocation,'TestData.mat'))
                load(fullfile(TestCase.SLocation,'TestData.mat'),'TestData');
            else
                TestData = struct('Script',[],'NumberOfTests',[],'VariableNames',[],'Expected',[],...
                                    'Tolerance',[],'Actual',[],'ErrorPC',[],'ErrorPCofTol',[],...
                                        'Test',[]);
            end
            
            TestData(TestCase.TestNum).Script           = TestCase.Name;
            TestData(TestCase.TestNum).NumberOfTests    = TestCase.NumOfTests;
            TestData(TestCase.TestNum).VariableNames    = TestCase.VName;
            
            TestData(TestCase.TestNum).Expected         = TestCase.Expected;
            TestData(TestCase.TestNum).Tolerance        = TestCase.Tolerance;
            TestData(TestCase.TestNum).Actual           = TestCase.Actual;
            TestData(TestCase.TestNum).ErrorPC{length(TestData(TestCase.TestNum).ErrorPC)+1}...
                                                        = TestCase.ErrorPC;
            TestData(TestCase.TestNum).ErrorPCofTol{length(TestData(TestCase.TestNum).ErrorPCofTol)+1}...
                                                        = TestCase.ErrorPCT;
            
            save(fullfile(TestCase.SLocation,'TestData.mat'),'TestData');
        end
    end
    
    methods (Test)                                              %Remove Tests below as appropriate for the number of variables
        function UnitTest1(TestCase)
            [r,c] = size(TestCase.Actual{1});
            for j = 1:r
                for i = 1:c
                    TestCase.verifyEqual(TestCase.Actual{1}(j,i),TestCase.Expected{1}(j,i),'AbsTol',TestCase.Tolerance{1});
                
                    TestCase.ErrorPC(j,i)  = (abs(TestCase.Actual{1}(j,i)-TestCase.Expected{1}(j,i))/TestCase.Expected{1}(j,i))*100;
                    TestCase.ErrorPCT(j,i) = (abs(TestCase.Actual{1}(j,i)-TestCase.Expected{1}(j,i))/TestCase.Tolerance{1})*100;
                end
            end
        end
        function UnitTest2(TestCase)
            [r,c] = size(TestCase.Actual{2});
            for j = 1:r
                for i = 1:c
                    TestCase.verifyEqual(TestCase.Actual{2}(j,i),TestCase.Expected{2}(j,i),'AbsTol',TestCase.Tolerance{2});
                
                    TestCase.ErrorPC(j,i)  = (abs(TestCase.Actual{2}(j,i)-TestCase.Expected{2}(j,i))/TestCase.Expected{2}(j,i))*100;
                    TestCase.ErrorPCT(j,i) = (abs(TestCase.Actual{2}(j,i)-TestCase.Expected{2}(j,i))/TestCase.Tolerance{2})*100;
                end
            end
        end
        function UnitTest3(TestCase)
            [r,c] = size(TestCase.Actual{3});
            for j = 1:r
                for i = 1:c
                    TestCase.verifyEqual(TestCase.Actual{3}(j,i),TestCase.Expected{3}(j,i),'AbsTol',TestCase.Tolerance{3});
                
                    TestCase.ErrorPC(j,i)  = (abs(TestCase.Actual{3}(j,i)-TestCase.Expected{3}(j,i))/TestCase.Expected{3}(j,i))*100;
                    TestCase.ErrorPCT(j,i) = (abs(TestCase.Actual{3}(j,i)-TestCase.Expected{3}(j,i))/TestCase.Tolerance{3})*100;
                end
            end
        end
        function UnitTest4(TestCase)
            [r,c] = size(TestCase.Actual{4});
            for j = 1:r
                for i = 1:c
                    TestCase.verifyEqual(TestCase.Actual{4}(j,i),TestCase.Expected{4}(j,i),'AbsTol',TestCase.Tolerance{4});
                
                    TestCase.ErrorPC(j,i)  = (abs(TestCase.Actual{4}(j,i)-TestCase.Expected{4}(j,i))/TestCase.Expected{4}(j,i))*100;
                    TestCase.ErrorPCT(j,i) = (abs(TestCase.Actual{4}(j,i)-TestCase.Expected{4}(j,i))/TestCase.Tolerance{4})*100;
                end
            end
        end
    end
    
end
