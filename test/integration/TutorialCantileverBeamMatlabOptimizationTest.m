classdef TutorialCantileverBeamMatlabOptimizationTest < TutorialTest
    
    properties
        TutorialName  = 'TutorialCantileverBeamMatlabOptimization';
        CoutputNames  = {'SQP(5)' 'COBYLA(5)' 'GA(5)'};    
        CvaluesExpected = {1.01e-07   2.6385e-05   9.9860e-04};      
        Ctolerance   = {1e-4 1e-4 1e-4};        
        PreTest     = {};
    end
    
end      