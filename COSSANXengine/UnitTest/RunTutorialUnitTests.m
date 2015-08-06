% Reduce the output on console 
OpenCossan.setVerbosityLevel(0);

addpath Tutorials
clc;

import matlab.unittest.TestSuite;
Test = TestSuite.fromFolder('Tutorials');
Result = run(Test);
close all;


load(fullfile(pwd,'TestData.mat'),'TestData');

c = 0;
for i = 1:length(TestData)
    for c = c+1:c+TestData(i).NumberOfTests
        TestData(i).Test = [TestData(i).Test, Result(c)];
    end
end

save(fullfile(pwd,'TestData.mat'),'TestData');