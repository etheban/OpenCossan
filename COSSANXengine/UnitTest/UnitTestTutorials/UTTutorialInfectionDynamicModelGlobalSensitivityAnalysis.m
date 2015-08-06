classdef UTTutorialInfectionDynamicModelGlobalSensitivityAnalysis < matlab.unittest.TestCase                          %Enter Script name after UT
    
    properties
        TestNum     = 14;                                         %Enter Test Number
        PreTest     = {[]};                                     %Enter the names of any scripts that need to be run
                                                                %prior to performing the test into a cell array.
        Name        = 'TutorialInfectionDynamicModelGlobalSensitivityAnalysis';                                       %Enter the name of the script to be tested.
        VName       = {'Xsm1.VsobolFirstIndices''' 'Xsm2.VtotalIndices'...
                        'Xsm2.VsobolFirstIndices' 'Xsm3.VupperBounds'};                                     %Enter Variable Names into cell array
        Expected    = {[4.3600e-01 4.1978e-01 1.0654e-02 8.7441e-03] ...
                       [4.2428e-02 6.0280e-01 4.2346e-02 5.7005e-01] ...
                       [1.7882e-03 4.4173e-01 2.0582e-03 4.6706e-01] ...
                       [6.4022e+01 6.6431e+01 8.5507e-04 8.4583e-04]};                                     %Enter the expected results into cell array
        Tolerance   = {0.0001 0.000001 0.0001 0.001};                                     %Enter the given tolerances into cell array
        NumOfTests  = 4;                                         %Enter the number of tests being performed (no. of variables)
        
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
