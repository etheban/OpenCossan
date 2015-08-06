%*********************************************************************
%
%   Example on how to use CrackGrowth objects
%   CrackGrowth is a subclass of Mio and shares various attributes and methods
%   with the mother class
%   This tutorial shows the usage of the methods in the class CrackGrowth. 
%
%**************************************************************************

% Definition of the CrackGrowth object. It takes as an inpout the outputs
% of the evaluator which determines the stress intensity factor, The
% outputs of this objects are the variations of the crack length over one
% cycle
Xcg = CrackGrowth('Lfunction',true,'Liostructure',true,'Liomatrix',false,...
    'Cinputnames',{'sif','m','C'},... % Define the inputs
    'Spath','./',...
    'Sfile','ParisErdogan.m',... % external file
    'Coutputnames',{'dadn'});


%% evaluate
% creation of a structure
Tstruct = struct;
Tstruct.sif =30e6;
Tstruct.m =2;
Tstruct.C =2e-23;
% evaluate
resu = Xcg.evaluate(Tstruct);
