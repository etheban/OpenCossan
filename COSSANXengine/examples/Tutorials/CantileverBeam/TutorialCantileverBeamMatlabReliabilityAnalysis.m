%% TutorialCantileverBeam: Reliability Analysis
% Perform a reliability analysis on a cantilever beam. The 
% documentation and the problem description of this example is available 
% at: <http://cossan.co.uk/wiki/index.php/Cantilever_Beam>
%
% <<cantilever-beam.png>>
%
% Author: *Edoardo Patelli*, Institute for Risk and Uncertainty, University
% of Liverpool, UK

%% LICENSE
%{
This file is part of OpenCossan <https://cossan.co.uk>.
Copyright (C) 2006-2018 COSSAN WORKING GROUP

OpenCossan is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License or,
(at your option) any later version.
	
OpenCossan is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with OpenCossan. If not, see <http://www.gnu.org/licenses/>.
%}


%% Setup
% This tutorial requires the Model constructed in the tutorial 
% <TutorialCantileverBeamMatlab.html>

assert(logical(exist('XmodelBeamMatlab','var')),'openCOSSAN:Tutorial', ...
    'Please run the tutorial TutorialCantileverBeamMatlab first')

% Reset the random number generator in order to always obtain the same
% results. *DO NOT CHANGE THE VALUES OF THE SEED!*
OpenCossan.resetRandomNumberGenerator(51125);

% Set the verbosity level to 2 in order to silence evaluator output
% messages.
OpenCossan.setVerbosityLevel(2);

%% Definition of the Probabilistic Model
% Construct the |PerformanceFunction|
Xperfun = PerformanceFunction('Sdemand','w','Scapacity','maxDisplacement','Soutputname','Vg');
% Construct the ProbabilisticModel from the Model and the
% PerformanceFunction
XprobModelBeamMatlab = ProbabilisticModel('Xmodel',XmodelBeamMatlab,'XperformanceFunction',Xperfun);

%% Reliability Analysis: Monte Carlo Sampling
% Estimate the probability of failure using Monte Carlo simulation.

% Definition of the Simulation Object
Xmc = MonteCarlo('Nsamples',1e4,'Nbatches',1);

% Run Reliability Analysis
XfailureProbMC = Xmc.computeFailureProbability(XprobModelBeamMatlab);
% Display the estimated failure probability
display(XfailureProbMC);

% Validate Solution
 assert(abs(XfailureProbMC.pfhat-7.38e-02)<eps,...
    'CossanX:Tutorials:CantileverBeam','Reference Solution pf MCS not matched.')

%% Reliability Analysis: Latin Hypercube Sampling
% Estimate the probability of failure using Latin Hypercube Sampling

% Definition of the Simulation object
Xlhs = LatinHypercubeSampling('Nsamples',1e3);

% Run Reliability Analysis
XfailureProbLHS = Xlhs.computeFailureProbability(XprobModelBeamMatlab);
% Display the estimated failure probability
display(XfailureProbLHS);

% Validate Solution
assert(abs(XfailureProbLHS.pfhat-8.30e-02)<eps,...
    'CossanX:Tutorials:CantileverBeam','Reference Solution pf LHS not matched.')

%% Reliability Analysis: Line Sampling
% Line Sampling requires the definition of the so-called important direction.
% It can be computed using the sensitivity method. For instance here the Local
% Sensitivity Analysis is computed.

XlsFD = LocalSensitivityFiniteDifference('Xmodel',XprobModelBeamMatlab,'Coutputname',{'Vg'});
display(XlsFD);

% Compute the LocalSensitivityMeasure
XlocalSensitivity = XlsFD.computeIndices;

OpenCossan.resetRandomNumberGenerator(49564);
% Use sensitivity information to define the important direction for LineSampling
XLS = LineSampling('XlocalSensitivityMeasures',XlocalSensitivity,'Nlines',50);
% Run Reliability Analysis
[XfailureProbLS, Xout]=XLS.computeFailureProbability(XprobModelBeamMatlab);
% Display results
display(XfailureProbLS);
display(Xout);

% Validate Solution
assert(abs(XfailureProbLS.pfhat-6.085e-002)<2e-5,...
    'CossanX:Tutorials:CantileverBeam',...
    'Estimated failure probability (%e) does not match the reference Solution (%e)',...
    XfailureProbLS.pfhat,6.1e-002)

%% Plot Line Sampling Results
% Plot lines 
f1 = Xout.plotLines;

%% Close figure
close(f1);

%% Reliability Analysis: Adaptive Line Sampling

% Line Sampling with adaptive method
OpenCossan.resetRandomNumberGenerator(1241243);
XALS = AdaptiveLineSampling('Nlines',50);
XfailureProbLS2 = XALS.computeFailureProbability(XprobModelBeamMatlab);

% Display estimated failure probability
display(XfailureProbLS2);

% Validate Solution
assert(abs(XfailureProbLS2.pfhat-5.992e-02)<1e-4,...
    'CossanX:Tutorials:CantileverBeam',...
    'Estimated failure probability (%e) does not match the reference Solution (%e)',...
    XfailureProbLS2.pfhat,5.992e-02)