%% TutorialOPTIMUM
%
% This Tutorial shows how to use the object Optimim
% 
% See Also:  http://cossan.cfd.liv.ac.uk/wiki/index.php/@Optimum
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author: Edoardo-Patelli$ 

% Reset the random number generator in order to obtain always the same results.
% DO NOT CHANGE THE VALUES OF THE SEED
OpenCossan.resetRandomNumberGenerator(56236)


%%  Problem Definition 
% Define Input parameters
% Random Variables
Xrv1    = RandomVariable('Sdistribution','normal', 'mean',1,'std',0.5); 
Xrv2    = RandomVariable('Sdistribution','normal', 'mean',-1,'std',2);
Xrv3    = RandomVariable('Sdistribution','normal', 'mean',10,'std',0.5); 
Xrv4    = RandomVariable('Sdistribution','normal', 'mean',10,'std',2);
C       = Parameter('Sdescription','Capacity','value',3);

%  RandomVariableSets 
Xrvs1   = RandomVariableSet('Cmembers',{'Xrv1','Xrv3','Xrv4'},'Cxrandomvariables',{Xrv1 Xrv3 Xrv4}); 
Xrvs2   = RandomVariableSet('Cmembers',{'Xrv2'},'Cxrandomvariables',{Xrv2}); 

% Define Input
Xin     = Input('CXmembers',{Xrvs1 Xrvs2 C},'CSmembers',{'Xrvs1' 'Xrvs2' 'C'});

%  Construct Mio object
Xm      = Mio('Sdescription','normalized demand', ...
    'Sscript','for i=1:length(Tinput),Toutput(i,1).D = 1/sqrt(2)*((Tinput(i).Xrv1-1)/0.5+(Tinput(i).Xrv2+1)/2);end',...
    'Liostructure',true,...
    'Lfunction',false,...
    'Cinputnames',{'Xrv1','Xrv2'},...
    'Coutputnames',{'D'});            

%  Construct evaluator
Xeval   = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');
%   Define Physical Model
Xmdl    = Model('Xevaluator',Xeval,'Xinput',Xin);

%% Define Probabilistic Model
% Define Performance Function
Xperf   = PerformanceFunction('Scapacity','C','Sdemand','D','Soutputname','Vg');
% Construct a probmodel
Xpm     = ProbabilisticModel('XModel',Xmdl,'XPerformanceFunction',Xperf);

%% Identify design point 
% The design point is identified using a HLRF like algorithms, as default
% method. It identify the design point by means of linear approximation, that
% means that the method is very efficient when the limit state function is
% linear or slightly non-linear. 

[XdesignPoint  Xoptimum] = Xpm.designPointIdentification;

%% Plot resutls
% Plot objective function 
f1=Xoptimum.plotObjectiveFunction;
% Plot contraints
f2=Xoptimum.plotConstraint;
% Plot design variables
f3=Xoptimum.plotDesignVariable;

%% Close figures
close(f1), close(f2),close(f3)

% Validate Solution
Vreference=3.481519e+00;
assert(abs(XdesignPoint.ReliabilityIndex-Vreference)<1e-4,...
    'CossanX:Tutorials:TutorialDesignPointIdentification','Reference Solution does not match.')

% Show summary
display(Xoptimum)

