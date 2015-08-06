classdef UnitTestsTutorials < matlab.unittest.TestCase
    
    properties
        TutorialList = [];
        TestData     = struct('TutorialName',[],'VariableNames',[],'Expected',[],'Tolerance',[],'Actual',[],...
                                        'ErrorPC',[],'ErrorPCofTol',[],'TestResults',[]);
    end
    
    methods (TestMethodSetup)
        function GetTutorialList(TestCase)
            
%             L1 = dir(fullfile('..','Tutorials'));
%             L1(1:2)=[];                                                           % First 2 are always '.' and '..' 
%             L1(~[L1.isdir])=[];                                                   % Remove non folders 
%             for n = 1:length(L1)
%                 L2= dir(fullfile('..','Tutorials',L1(n).name,'Tutorial*.m'));
%                 TestCase.TutorialList=[TestCase.TutorialList; L2(~[L2.isdir])];   % remove directories (~[L2.isdir]) and merges results
%             end
            TestCase.TutorialList = struct('name','TutorialExample.m');
        end
    end

    methods (TestMethodTeardown)
        function SaveData(TestCase)
            TestData = TestCase.TestData;
            save(fullfile(pwd,'TestData.mat'),'TestData');
        end
    end
    
    methods (Test)
        function TutorialUnitTests(TestCase)
            for k = 1:length(TestCase.TutorialList)
                try
                    eval(TestCase.TutorialList(k).name(1:(length(TestCase.TutorialList(k).name)-2)));
                catch ME
                    ME
                end
                
                for m = 1:length(TutUnitTest.Actual)
                    [r,c] = size(TutUnitTest.Actual{m});
                    for j = 1:r
                        for i = 1:c
                            TestCase.verifyEqual(TutUnitTest.Actual{m}(j,i),TutUnitTest.Expected{m}(j,i),'AbsTol',TutUnitTest.Tolerance{m});
                
                            if abs(TutUnitTest.Actual{m}(j,i) - TutUnitTest.Expected{m}(j,i)) < TutUnitTest.Tolerance{m}
                                TestResults{m} = true;
                            else
                                TestResults{m} = false;
                            end
                            ErrorPC{m}(j,i)  = (abs(TutUnitTest.Actual{m}(j,i)-TutUnitTest.Expected{m}(j,i))/TutUnitTest.Expected{m}(j,i))*100;
                            ErrorPCT{m}(j,i) = (abs(TutUnitTest.Actual{m}(j,i)-TutUnitTest.Expected{m}(j,i))/TutUnitTest.Tolerance{m})*100;                        
                        end
                    end
                end
                
                TestCase.TestData(k).TutorialName   = TestCase.TutorialList(k).name;
                TestCase.TestData(k).VariableNames  = TutUnitTest.VariableNames;
                TestCase.TestData(k).Expected       = TutUnitTest.Expected;
                TestCase.TestData(k).Tolerance      = TutUnitTest.Tolerance;
                TestCase.TestData(k).Actual         = TutUnitTest.Actual;
                TestCase.TestData(k).ErrorPC        = ErrorPC;
                TestCase.TestData(k).ErrorPCofTol   = ErrorPCT;
                TestCase.TestData(k).TestResults    = TestResults;

            end
        end
    end
    
end
