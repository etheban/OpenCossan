%% Tutorial for the Constraint object
% The Constrains object defines the constains for the
% optimization problem. It is a subclass of the Mio object and inherits all
% the methods from that class. 
% Please refer to the Mio tutorial and Optimization tutorial  for more
% examples of the constraints
%
% See Also:  http://cossan.cfd.liv.ac.uk/wiki/index.php/@Constraint
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Edoardo~Patelli$ 

%% Constructor
Xcon   = Constraint('Sdescription','non linear inequality constraint', ...
    'Sscript','for n=1:length(Tinput),Toutput(n).Con1=2-Tinput(n).X1-Tinput(n).X2;end',...
    'Coutputnames',{'Con1'},'Cinputnames',{'X1','X2'},'Linequality',true);

% The constrains object requires the fieds Linequality to define the type
% of constaints.


% Show details of the Constraints
display(Xcon)

%% Test Constructor
% a structure of inputs values is required to evaluate the Constraint object
Tinput.X1=4;
Tinput.X2=3;

Xout=Xcon.run(Tinput);

display(Xout);

%% Define an optimization problem 
% Create input 
Xin     = Input;

X1      = DesignVariable('Sdescription','design variable 1','value',7);
X2      = DesignVariable('Sdescription','design variable 2','value',2);
Xin     = add(Xin,X1);
Xin     = add(Xin,X2);

Xofun   = ObjectiveFunction('Sdescription','objective function', ...
          'Cinputnames',{'X1','X2'},... % Define the inputs 
          'Sscript','for n=1:length(Tinput),Toutput(n).fobj=Tinput(n).X1;end',...
          'Coutputnames',{'fobj'}); % Define the outputs


Xop     = OptimizationProblem('Sdescription','Optimization problem', ...
    'Xinput',Xin,'XobjectiveFunction',Xofun,'Xconstraint',Xcon);

% It is necessary to initialize a Optimum object before evaluating
% Contraints and Objective function
%
Xoptimum=initializeOptimum(Xop);

% Evaluate the objective fuction at 2 points: 5 4 and 2 1.
Vo=Xcon.evaluate('Xoptimum',Xoptimum,'XoptimizationProblem',Xop,'MreferencePoints',[5 4; 2 1]);
[Vo, ~, Mgrad]=Xcon.evaluate('Xoptimum',Xoptimum,'XoptimizationProblem',Xop,'MreferencePoints',[5 4],'Lgradient',true);

%% More constrains can be defined using the multiple Constraints object.
Xcon   = Constraint('Sdescription','2 non linear inequality constraints and 1 equality constain', ...
    'Sscript','for n=1:length(Tinput),Toutput(n).Con1=2-Tinput(n).X1-Tinput(n).X2;end',...
    'Coutputnames',{'Con1'},'Cinputnames',{'X1','X2'},'Linequality',true);

Xcon(2)   = Constraint('Sdescription','2 non linear inequality constraints and 1 equality constain', ...
    'Sscript','for n=1:length(Tinput),Toutput(n).Con2=Tinput(n).X1;end',...
    'Coutputnames',{'Con2'},'Cinputnames',{'X1','X2'},'Linequality',true);

Xcon(3)   = Constraint('Sdescription','2 non linear inequality constraints and 1 equality constain', ...
    'Sscript','for n=1:length(Tinput);Toutput(n).Con3=Tinput(n).X2;end',...
    'Coutputnames',{'Con3'},'Cinputnames',{'X1','X2'},'Linequality',false);


% Evaluate the objective fuction at 2 points: 5 4 and 2 1.
% Remove global variables

[Vin Veq]=Xcon.evaluate('Xoptimum',Xoptimum,'XoptimizationProblem',Xop,'MreferencePoints',[5 4; 2 1]);

[Vin Veq VinGrad VeqGrad]=Xcon.evaluate('XoptimizationProblem',Xop,'MreferencePoints',[5 4;],'Lgradient',true);

% Validate Solution
VinR=[-7 5];
VeqR=4;
VinGradR=[-1 1; -1 0];
VeqGradR=[0;1];

assert(max(abs(Vin-VinR))<1e-6,'CossanX:Tutorials:TutorialConstraint', ...
    'Reference Solution Inequality constraint does not match.')
assert(max(abs(Veq-VeqR))<1e-6,'CossanX:Tutorials:TutorialConstraint',...
    'Reference Solution Equality constraintdoes not match.')
assert(max(max(abs(VinGrad-VinGradR)))<1e-6,'CossanX:Tutorials:TutorialConstraint',...
    'Reference Solution Inequality constraint Gradient does not match.')
assert(max(max(abs(VeqGrad-VeqGradR)))<1e-6,'CossanX:Tutorials:TutorialConstraint',...
    'Reference Solution Equality constraint Gradient does not match.')
