function display(Xobj)
%DISPLAY  Displays the summary of the  Constraint  object
%
% =======================================================================
% COSSAN - COmputational Simulation and Stochastic ANnalysis
% University of Innsbruck, Copyright 1993-2011 IfM
% =======================================================================

%%  Output to Screen
%  Name and description
OpenCossan.cossanDisp('===================================================================',2);
OpenCossan.cossanDisp([' Constraint Object - Description: ' Xobj.Sdescription ],1);
OpenCossan.cossanDisp('===================================================================',2);

if isempty(Xobj.Coutputnames)
    OpenCossan.cossanDisp(' * Empty object ',1)
else
    
    OpenCossan.cossanDisp([' * Input Variables: ' sprintf('%s; ',Xobj.Cinputnames{:})],2)
    
    if Xobj.Linequality
        OpenCossan.cossanDisp(sprintf(' * Inequality constraint: %s ',Xobj.Coutputnames{1}),2)
    else
        OpenCossan.cossanDisp(sprintf(' * Equality constraint: %s ',Xobj.Coutputnames{1}),2)
    end
end


