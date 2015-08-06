% Unit testing of Optimization Toolbox
% BG

function varargout=UnitTestBFGS

Ntest=8;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Predefine object

itest=1;
try
    x1       = DesignVariable('value',5,'lowerBound',0,'upperBound',10);
    x2       = DesignVariable('value',5,'lowerBound',0,'upperBound',10);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2); 
    Xobjfunc1 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=100*(Tinput(i).x2-Tinput(i).x1^2)^2+(1-Tinput(i).x1)^2; end', ...
        'Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'}, ...
        'Liostructure',true);
    Xcons1 = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).cons1=Tinput(i).x1 + Tinput(i).x2-5; end', ...
        'Cinputnames',{'x1','x2'},...
        'Coutputnames',{'cons'}, ...
        'Liostructure',true);
   
    Xoptprob = OptimizationProblem('Xinput',Xin, ...
        'CXobjectiveFunctions',{Xobjfunc1},...
        'VinitialSolution',[-1.2 1]);
    Xoptprob2 = OptimizationProblem('Xinput',Xin, ...
        'CXobjectiveFunctions',{Xobjfunc1},...
        'Xconstraint',Xcons1, ...
        'VinitialSolution',[-1.2 1]);
    
    Cmess{itest}='Object initialized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object

itest = itest+1;
try
    Xobj = BFGS;
    display(Xobj);
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 3 apply method

itest = itest+1;
try
    Xobj = BFGS;
    Xout1 = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(abs(Xout1.getOptimalObjective)<0.1,'Wrong result for objective function')
    Cmess{itest}='Check apply method';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 apply method using NmaxModelEvaluations 

itest = itest+1;
try
    Xobj = BFGS('NmaxModelEvaluations',5);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(strcmp(Xout.Sexitflag,'Number of iterations exceeded options.MaxIter or number of function evaluations exceeded options.MaxFunEvals'),'Option NmaxModelEvaluations not used')
    Cmess{itest}='Check apply method using NmaxModelEvaluations ';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 apply method using NmaxIterations

itest = itest+1;
try
    Xobj = BFGS('NmaxIterations',5);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(strcmp(Xout.Sexitflag,'Number of iterations exceeded options.MaxIter or number of function evaluations exceeded options.MaxFunEvals'),'Option NmaxModelEvaluations not used')
    Cmess{itest}='Check apply method using NmaxIterations';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 6 apply method using toleranceDesignVariables and toleranceObjectiveFunction

itest = itest+1;
try
    Xobj = BFGS('toleranceObjectiveFunction',0.1,'toleranceDesignVariables',0.1);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(Xout1.getOptimalObjective<Xobj.toleranceObjectiveFunction,'Option toleranceDesignVariables not satisftied')
    Cmess{itest}='Check apply method using toleranceDesignVariables and toleranceObjectiveFunction';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 apply method using finiteDifferencePerturbation 

itest = itest+1;
try
    % TODO: this should be fixed
    Xobj = BFGS('finiteDifferencePerturbation',0.00001);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(abs(Xout.getOptimalObjective)<1.e-4,'Option finiteDifferencePerturbation ignored')
    Cmess{itest}='Check apply method using finiteDifferencePerturbation';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 apply method using a constraint optimization problem

itest = itest+1;
try
    Xobj = BFGS;
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob2);
    Cmess{itest}='This test shall fail since BFGS is an UNconstraint optimization method';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Finish the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='BFGS';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of BFGS (' datestr(now) ')'])
    disp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
    
end
