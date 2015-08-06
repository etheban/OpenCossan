% Unit testing of Optimization Toolbox
% BG

function varargout=UnitTestSimplex

Ntest=6;
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
   
    Xoptprob = OptimizationProblem('Xinput',Xin, ...
        'CXobjectiveFunctions',{Xobjfunc1},...
        'VinitialSolution',[-1.2 1]);
    Cmess{itest}='Object initialized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object

itest = itest+1;
try
    Xobj = Simplex;
    display(Xobj);
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 3 apply method

itest = itest+1;
try
    Xout1 = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(abs(Xout1.XdesignVariable.Mdata(1,end)-1.0)<1.e-2,'Wrong result for x1')
    assert(abs(Xout1.XdesignVariable.Mdata(2,end)-1.0)<1.e-2,'Wrong result for x2')
    Cmess{itest}='Check apply method';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 apply method using NmaxModelEvaluations 

itest = itest+1;
try
    Xobj = Simplex('NmaxModelEvaluations',5);
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
    Xobj = Simplex('NmaxIterations',5);
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
    Xobj = Simplex('toleranceObjectiveFunction',0.1,'toleranceDesignVariables',0.1);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(Xout.NevaluationsObjectiveFunction<Xout1.NevaluationsObjectiveFunction,'Option toleranceDesignVariables ignored')
    Cmess{itest}='Check apply method using toleranceDesignVariables and toleranceObjectiveFunction';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% Finish the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='Simplex';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Simplex (' datestr(now) ')'])
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
