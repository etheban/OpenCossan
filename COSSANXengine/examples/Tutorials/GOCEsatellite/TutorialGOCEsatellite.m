%% GOCE satellite Tutorial
% 
% In this tutorial, the first eigenfrequency and the first term of the
% diagonal terms of the MAC-matrix are approximated by neural networks.
% These neural networks are then used for sensitivity analysis
%
% See Also: http://cossan.cfd.liv.ac.uk/wiki/index.php/GOCE_satellite
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~ BG$ 

% Reset the random number generator in order to always obtain the same results.
% DO NOT CHANGE THE VALUES OF THE SEED
OpenCossan.resetRandomNumberGenerator(51125)

%% Definition of the Neural Networks

% load file which contains the Input objects and Simulation Data objects used for calibration and validation 
Sdirectory = fileparts(which('TutorialGOCESatellite.m'));
load(fullfile(Sdirectory,'NeuralNetworkInput.mat')); 

% Neural network for 1st eigenfrequency
XnnFREQ1 = NeuralNetwork('Sdescription','1st eigenfrequency of GOCE satellite',...
    'Stype','HyperbolicTangent',...
    'Coutputnames',{'Freq1'},...  %response to be extracted from full model
    'Vnnodes',[18, 7, 1],... % number of layers used within the NN
    'Vnormminmax',[-0.8 0.8],... % normalization factors
    'Xcalibrationinput', Xincalibration,... % Input object with calibration points
    'Xcalibrationoutput',Xoutcalibration,...% SimulationData object with model evaluations at calibration points
    'Xvalidationinput',Xinvalidation, ...% Input object with validation points
    'Xvalidationoutput',Xoutvalidation); % SimulationData object with model evaluations ar validation points

% Neural network for 1st term of MAC-matrix
XnnMAC1 = NeuralNetwork('Sdescription','1st MAC term of GOCE satellite',...
    'Stype','HyperbolicTangent',...
    'Coutputnames',{'DiagMAC1'},...  %response to be extracted from full model
    'Vnnodes',[18, 7, 1],... % number of layers used within the NN
    'Vnormminmax',[-0.8 0.8],... % normalization factors
    'Xcalibrationinput', Xincalibration,... % Input object with calibration points
    'Xcalibrationoutput',Xoutcalibration,... % SimulationData object with model evaluations at calibration points
    'Xvalidationinput',Xinvalidation, ... % Input object with validation points
    'Xvalidationoutput',Xoutvalidation); % SimulationData object with model evaluations ar validation points


%% Regression plots of 1st eigenfrequency and 1st diagonal term of MAC-matrix

% regression plot of validation samples for 1st eigenfrequency
f1 = figure;
XnnFREQ1.plotregression('Stype','validation','Soutputname','Freq1');

% regression plot of validation samples for 1st diagonal term of MAC-matrix
f2 = figure;
XnnMAC1.plotregression('Stype','validation','Soutputname','DiagMAC1');


%% Sensitivity analysis of 1st eigenfrequency and 1st diagonal term of MAC-matrix

Nsamples=2e4; % number of samples
Xlh=MonteCarlo('Nsamples',Nsamples); % define Simulations object

% compute sensitivity measures for both the eigenfrequency and MAC-term
XsmRBD_FREQ1=Sensitivity.randomBalanceDesign('Xmodel',XnnFREQ1,'Nsamples',Nsamples,'Coutputnames',{'Freq1'});
XsmRBD_DiagMAC1=Sensitivity.randomBalanceDesign('Xmodel',XnnFREQ1,'Nsamples',Nsamples,'Coutputnames',{'Freq1'});

%% Plot figures of Sensitivity measures

XsmRBD_FREQ1.plot;
XsmRBD_DiagMAC1.plot;

%% validate solution

close(f1);
close(f2);

assert(all(all(abs(XsmRBD_FREQ1.VsobolFirstIndices(1:10)'-[4.1623e-04, 5.4004e-04, 1.0516e-03, 5.8427e-04 ...
        4.5886e-04, 4.6985e-04, 1.1752e-03, 7.2778e-04, 5.3344e-02, 4.2141e-01])<1e-4)), ...
       'CossanX:Tutorials:TutorialDataseries', ...
       'Reference Solution of first Sobol Indices (Freq1) does not match.')

assert(all(all(abs(XsmRBD_FREQ1.VsobolFirstIndices(1:10)'-[4.1623e-04, 5.4004e-04, 1.0516e-03, 5.8427e-04, ...        
        4.5886e-04, 4.6985e-04, 1.1752e-03, 7.2778e-04, 5.3344e-02, 4.2141e-01])<1e-4)), ...
       'CossanX:Tutorials:TutorialDataseries', ...
       'Reference Solution of first Sobol Indices (DiagMAC1) does not match.')





