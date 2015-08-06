%
% Building under wind loading - perform Monte Carlo simulation 
%
% In this example, the variation of the stresses of a structural model with
% random model parameters are computed by means of MonteCarlo simulation.
%
% The structural model comprises a 6-story building under a lateral wind 
% excitation. The load is modeled with deterministic constant forces acting 
% on a side of the building, with the pressure of the wind, and thus the
% acting force, increasing as the height of the building according to a 
% power increase.
% The material and geometric parameters of the columns, stairs ceiling and 
% floors of the building are modeled with independent random variables.
% 500 MonteCarlo simulations are carried out. 
%
% =========================================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2009 IfM
% =========================================================================

%% Initialization
% remove variables from the workspace and clear the console
clear variables


CossanX('Nverboselevel',3)

%% Load Physical Model
load Xmodel_Building

%% Define performance function
Xmio = Mio('Sscript','for i=1:length(Tinput); Toutput(i).Vg1=Tinput(i).Xresistance - max([Tinput(i).C2286_1 Tinput(i).C2286_2 Tinput(i).C2286_3 Tinput(i).C2286_4]);end;',...
           'Cinputnames',{'Xresistance' 'C2286_1' 'C2286_2' 'C2286_3' 'C2286_4'}, ...
           'Coutputnames',{'Vg1'});
       
Xpf=PerformanceFunction('Xmio',Xmio);

%% Define probabilistic model
Xpm=ProbabilisticModel('Xmodel',Xm,'XPerformanceFunction',Xpf);

tic
Xg=Sensitivity.localFiniteDifferences('Xtarget',Xpm,'Coutputname',{'Vg1'},'perturbation',0.1);
time_parallel = toc;

save('Xgradient_Building','Xg','time_parallel','Xpm','Xpf')

%% Clear files
delete abaqus* 


%%