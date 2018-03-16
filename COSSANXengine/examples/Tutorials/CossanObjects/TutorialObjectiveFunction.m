%% Tutorial for the objective function
% The ObjectiveFunction object defines the objective function for the
% optimization problem. It is a subclass of the Mio object and inherits all
% the methods from this class. 
% Please refer to the Mio tutorial and Optimization tutorial  for more
% examples of objective function
%
% See Also: http://cossan.co.uk/wiki/index.php/@ObjectiveFunction
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Edoardo~Patelli$ 

%% Constructor
Xofun   = ObjectiveFunction('Sdescription','objective function', ...
         'Lfunction',true,'Liostructure',true,'Liomatrix',false,...
          'Cinputnames',{'X1','X2'},... % Define the inputs 
          'Afunction',@rastriginsfcn,...
          'Coutputnames',{'fobj'}); % Define the outputs

% Show details of the ObjectiveFunction
display(Xofun)

% The ObjectFunction can also be defined as a script.

ScurrentPath=which('TutorialObjectiveFunction');
[Spath, ~ ]=fileparts(ScurrentPath);
Xofun1  = ObjectiveFunction('Sdescription','objective function of optimization problem', ...
    'Spath',fullfile(Spath,'Files4Mio') ,...
    'Sfile','ExampleMioStructure',...
    'Liostructure',true,...
    'Lfunction',true,...
    'Cinputnames',{'X1','X2'},...
    'Coutputnames',{'mioout'});

display(Xofun1)


%% Use ObjectiveFunction
% In order to be able to use the method evaluate of ObjectiveFunction an
% OptimizationProblem needs to be defined.
Xofun1.evaluate
