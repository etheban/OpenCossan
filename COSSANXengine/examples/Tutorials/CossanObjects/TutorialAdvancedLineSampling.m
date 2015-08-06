%% Tutorial for the Advanced Line Sampling Object
%
% This tutorial tests the use of the Advanced Line Sampling method.
%
% See Also: http://cossan.cfd.liv.ac.uk/wiki/index.php/@AdvancedLineSampling
%
% $Author:~Marco~de~Angelis$ 

%% Example 3: Nonlinear limit state with saddle point
% Define the random variables of the problem
X1=RandomVariable('Sdistribution','normal', 'mean',0,'var',1);
X2=RandomVariable('Sdistribution','normal', 'mean',0,'var',1);

% Define the RandomVariableSet
Xrvs=RandomVariableSet('CSmembers',{'X1', 'X2'},...
    'CXrandomVariables',{X1, X2});
% Define the input object
Xinput = Input('Sdescription','Input Object', ...
    'CXmembers',{Xrvs},'CSmembers',{'Xrvs'});
%% Define the Evaluator (i.e. how our model is evaluated)
% Construct a Mio object
Xmio_model=Mio('Sdescription', 'Matlab I-O for the performance model',...
    'Sscript',...
    'Moutput=2-Minput(:,2)-0.1*Minput(:,1).^2+0.06*Minput(:,1).^3;',...
    'Liostructure',false,...
    'Liomatrix',true,...
    'Lfunction',false,...
    'Coutputnames',{'performance_function'},...
    'Cinputnames',{'X1' 'X2'});
% Construct the evaluator object
Xevaluator = Evaluator('Xmio',Xmio_model);
%% Define the physical model
Xmodel=Model('Xevaluator',Xevaluator,'Xinput',Xinput);
% Test the Model
% Generate 10 random realization of the input
Xinput = sample(Xinput,'Nsamples',10);
% Check the Model object
Xo = apply(Xmodel,Xinput);
% Show Results
display(Xo);
%% Define the Probabilistic Model
Xmio_performance=Mio('Sdescription', 'Matlab I-O for the performance function',...
    'Sscript','Moutput=Minput;',...
    'Liostructure',false,...
    'Liomatrix',true,...
    'Lfunction',false,...
    'Cinputnames',{'performance_function'},...
    'Coutputnames',{'Vg'});
% Create the performance function object
Xperformance=PerformanceFunction('Sdescription','My Performance Function', ...
    'Xmio',Xmio_performance);
% Construct the Probabilisti Model object
XprobModel=ProbabilisticModel('Sdescription','Defines our reliability analysis',...
    'Xmodel',Xmodel,'XperformanceFunction',Xperformance);
%% Define an Important Direction
% Construct the Local Sensitivity by Finite Difference
Xlsfd=LocalSensitivityFiniteDifference('Xtarget',XprobModel, ...
    'Coutputnames',{'Vg'});
% Compute the Gradient
Xgrad = Xlsfd.computeGradient;
ValphaGRA= -Xgrad.Valpha;
% Compute the Indeces
Xinde = Xlsfd.computeIndices;
ValphaLSM= -Xinde.Valpha;
%% Create the Advanced Line Sampling object
% Use direction computed in the standard normal space
% Xals1 = AdaptiveLineSampling('Nlines',30,...
%     'Vdirectionstandardspace',[-1;1]);
Xals1 = AdaptiveLineSampling('Nlines',60,...
    'Vdirectionphysical',[-1,1],'NeffectiveUpdates',5);
% % Use direction provided by the gradient in the original space
% Xals2 = AdvancedLineSampling('Nlines',30,'Valpha',ValphaGRA);
% % Use limit state points from previous simulations
% load Mlsp_example_3
% Xals3 = AdvancedLineSampling('Nlines',30,'Mstatepoints',Mlsp);
% % Use failure points from previous simulations
% load Mfail_example_3
% Xals4 = AdvancedLineSampling('Nlines',30,'MfailurePoints',Mfail);
%% Compute Reference Solution
% % This can take a little while
% Xmc=MonteCarlo('Nsamples',1e5);
% [Xpfmc,Xoutmc]=XprobModel.computeFailureProbability(Xmc);
% display(Xpfmc)
%% Reference solution
% pF_ref = 3.47e-2; (Der Kiureghian and Lin, 1987. J Eng Mech Div ASCE)
%% Estimate the Failure Probability
% Reset random number stream
OpenCossan.resetRandomNumberGenerator(51125) 
[Xpf,Xout]=XprobModel.computeFailureProbability(Xals1);
display(Xpf)
%% Post process the results
% Create Line Data output object
SperfName=XprobModel.XperformanceFunction.Soutputname;
XlineData=LineData('Sdescription','My first Line Data object',...
    'Xals',Xals1,'LdeleteResults',false,...
    'Sperformancefunctionname',SperfName,...
    'Xinput',Xinput);
% plot limit state
XlineData.plotLimitState('XsimulationData',Xout,'Xmodel',XprobModel);
XlineData.plotLines
% %% Plot results
% % plot lines
% Xout.plotLines('Stitle','Lines of the Performance Function');
% % plot limit state function
% Xout.plotLimitState('Stitle','','Vsupport',[-5,5],'Xmodel',XprobModel);