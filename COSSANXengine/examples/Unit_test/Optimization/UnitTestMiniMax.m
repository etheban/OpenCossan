% Unit testing of Optimization Toolbox
% BG

function varargout=UnitTestMiniMax

Ntest=7;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Predefine object

itest=1;
try
    x1       = DesignVariable('value',5,'lowerBound',0,'upperBound',10);
    x2       = DesignVariable('value',5,'lowerBound',0,'upperBound',10);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2);
    Xobjfunc1 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=2*Tinput(i).x1^2+Tinput(i).x2^2-48*Tinput(i).x1-40*Tinput(i).x2+304; end', ...
        'Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'}, ...
        'Liostructure',true);
    Xobjfunc2 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out2=-Tinput(i).x1^2-3*Tinput(i).x2^2; end', ...
        'Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out2'}, ...
        'Liostructure',true);
    Xobjfunc3 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out3=Tinput(i).x1+3*Tinput(i).x2 -18; end', ...
        'Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out3'}, ...
        'Liostructure',true);
    Xobjfunc4 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out4=-Tinput(i).x1-Tinput(i).x2; end', ...
        'Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out4'}, ...
        'Liostructure',true);
    Xobjfunc5 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out5=Tinput(i).x1+Tinput(i).x2-8; end', ...
        'Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out5'}, ...
        'Liostructure',true);
    
    Xoptprob = OptimizationProblem('Xinput',Xin, ...
        'CXobjectiveFunctions',{Xobjfunc1 Xobjfunc2 Xobjfunc3 Xobjfunc4 Xobjfunc5},...
        'VinitialSolution',[0.1 0.1]);
    Cmess{itest}='Object initialized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object

itest = itest+1;
try
    Xobj = MiniMax;
    display(Xobj);
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 3 apply method

itest = itest+1;
try
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(abs(Xout.XdesignVariable.Mdata(1,end)-4.0)<1.e-2,'Wrong result for x1')
    assert(abs(Xout.XdesignVariable.Mdata(2,end)-4.0)<1.e-2,'Wrong result for x2')
    Cmess{itest}='Check apply method';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 apply method using NmaxModelEvaluations 

itest = itest+1;
try
    Xobj = MiniMax('NmaxModelEvaluations',5);
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
    Xobj = MiniMax('NmaxIterations',5);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(strcmp(Xout.Sexitflag,'Number of iterations exceeded options.MaxIter or number of function evaluations exceeded options.MaxFunEvals'),'Option NmaxModelEvaluations not used')
    Cmess{itest}='Check apply method using NmaxIterations';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 6 apply method using toleranceDesignVariables

itest = itest+1;
try
    Xobj = MiniMax('toleranceDesignVariables',1,'toleranceConstraint',1);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(Xout.NevaluationsObjectiveFunction<13,'Option toleranceDesignVariables ignored')
    Cmess{itest}='Check apply method using toleranceDesignVariables';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 apply method using finiteDifferencePerturbation 

itest = itest+1;
try
    Xobj1 = MiniMax('finiteDifferencePerturbation',1.0);
    Xout1 = apply(Xobj1,'XoptimizationProblem',Xoptprob);
    assert(abs(Xout1.XobjectiveFunctionGradient(1).Mdata(1)+45.6)<1.e-4,'Option finiteDifferencePerturbation ignored')
    Cmess{itest}='Check apply method using finiteDifferencePerturbation ';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Finish the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='MiniMax';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of MiniMax (' datestr(now) ')'])
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
