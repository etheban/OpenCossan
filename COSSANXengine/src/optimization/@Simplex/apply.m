function [Xoptimum varargout] = apply(Xobj,varargin)
%   APPLY   This method applies the algorithm
%           Simplex for solving unconstrained problems
%
% ==================================================================
% COSSAN-X - The next generation of the computational stochastic analysis
% University of Innsbruck, Copyright 1993-2011 IfM
% ==================================================================

%% Define global variable for the objective function and the constrains
global XoptGlobal XsimOutGlobal

OpenCossan.validateCossanInputs(varargin{:});

%  Check whether or not required arguments have been passed
for k=1:2:length(varargin),
    switch lower(varargin{k}),
        case {'xoptimizationproblem'},   %extract OptimizationProblem
            if isa(varargin{k+1},'OptimizationProblem'),    %check that arguments is actually an OptimizationProblem object
                Xop     = varargin{k+1};
            else
                error('openCOSSAN:Simplex:apply',...
                    ['the variable  ' inputname(k) ' must be an OptimizationProblem object']);
            end
        case {'xoptimum'},   %extract OptimizationProblem
            if isa(varargin{k+1},'Optimum'),    %check that arguments is actually an OptimizationProblem object
                Xoptimum  = varargin{k+1};
            else
                error('openCOSSAN:Simplex:apply',...
                    ['the variable  ' inputname(k) ' must be an Optimum object']);
            end
        case 'vinitialsolution'
            VinitialSolution=varargin{k+1};
        otherwise
            error('openCOSSAN:Simplex:apply',['the field ' varargin{k} ...
                ' is not valid']);
    end
end

%% Check Optimization problem
assert(logical(exist('Xop','var')), 'openCOSSAN:Simplex:apply',...
    'Optimization problem must be defined')

% Check inputs and initialize variables
Xobj = initializeOptimizer(Xobj);

%% Check initial solution
if exist('VinitialSolution','var')
    Xop.VinitialSolution=VinitialSolution;
end

assert(size(Xop.VinitialSolution,1)==1, ...
    'openCOSSAN:Simplex:apply',...
    'Only 1 initial setting point is allowed')

%% initialize Optimum
if ~exist('Xoptimum','var')
    XoptGlobal=Xop.initializeOptimum('LgradientObjectiveFunction',false,'LgradientConstraints',false,...
                                     'Xoptimizer',Xobj);
else
    %TODO: Check Optimum 
    XoptGlobal=Xoptimum;
end


%%  Perform optimization
%   Set matlab options
Toptions            = optimset('fminsearch');  %Default optimization options
Toptions.Display    = 'iter-detailed';   %Turns on intermediate information about optimization procedure
Toptions.MaxFunEvals   = Xobj.Nmax;              %Maximum number of function evaluations that is allowed
Toptions.MaxIter       = Xobj.NmaxIterations;          %Maximum number of iterations that is allowed
Toptions.TolFun        = Xobj.toleranceObjectiveFunction;           %Termination tolerance on the value of the objective function
Toptions.TolX          = Xobj.toleranceDesignVariables;       %Termination tolerance on the design variable vector
Toptions.OutputFcn = @Xobj.outputFunctionOptimiser;

% initialize global variable
XsimOutGlobal=[];

if isempty(Xop.Xmodel)
    % Create handle of the objective function
    hobjfun=@(x)evaluate(Xop.XobjectiveFunction,'Xoptimizationproblem',Xop,...
        'MreferencePoints',x,'Lgradient',false,...        
        'scaling',Xobj.scalingFactor);
else
    % Create handle of the objective function
    hobjfun=@(x)evaluate(Xop.XobjectiveFunction,'Xoptimizationproblem',Xop,...
        'MreferencePoints',x,'Lgradient',false,...
        'scaling',Xobj.scalingFactor,'Xmodel',Xop.Xmodel);
end


assert(logical(isempty(Xop.Xconstraint)), ...
    'openCOSSAN:Simplex:apply',...
    'Simplex is an UNconstrained Nonlinear Optimization.')


% The function that computes the nonlinear inequality constraints c(x)≤ 0
% and the nonlinear equality constraints ceq(x) = 0. hconstrains accepts a
% vector x and returns the two vectors c and ceq. c is a vector that
% contains the nonlinear inequalities evaluated at x, and ceq is a vector
% that contains the nonlinear equalities evaluated at x.  hconstrains is
% a function handle such as function

OpenCossan.setLaptime('Sdescription',['SIMPLEX:' Xobj.Sdescription]);


%% Perform Real optimization
[~,~,Nexitflag]  = fminsearch(hobjfun,Xop.VinitialSolution,Toptions);

OpenCossan.setLaptime('Sdescription','End Simplex optimization');

%% Output
% All the quantities of interest are automatically stored in the Optimum
% object.

% Prepare string with reason for termination of optimization algorithm
switch Nexitflag
    case{1}
        Sexitflag   = 'Converged to a solution';
    case{0}
        Sexitflag   = 'Number of iterations exceeded options.MaxIter or number of function evaluations exceeded options.MaxFunEvals';
    case{-1}
        Sexitflag   = 'Algorithm was terminated by the output function';
end

XoptGlobal.Sexitflag=Sexitflag;

% Assign outputs
Xoptimum=XoptGlobal;

% Export Simulation Output
varargout{1}    = XsimOutGlobal; 

if ~isdeployed
    % add entries in simulation and analysis database at the end of the
    % computation when not deployed. The deployed version does this with
    % the finalize command
    XdbDriver = OpenCossan.getDatabaseDriver;
    if ~isempty(XdbDriver)
        XdbDriver.insertRecord('StableType','Result',....
            'Nid',getNextPrimaryID(OpenCossan.getDatabaseDriver,'Result'),...
            'CcossanObjects',{Xoptimum},...
            'CcossanObjectsNames',{'Xoptimum'});
    end
end
%% Delete global variables
clear global XoptGlobal XsimOutGlobal 

%%  Set random number generator to state prior to running simulation
if exist('XRandomNumberGenerator','var'),
    Simulations.restoreRandomNumberGenerator(XRandomNumberGenerator)
end

%% Record Time
OpenCossan.setLaptime('Sdescription','End apply@Simplex');
