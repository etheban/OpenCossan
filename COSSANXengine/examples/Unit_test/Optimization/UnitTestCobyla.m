% Unit testing of Optimization Toolbox

function varargout=UnitTestCobyla

Ntest=5;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Predefine object

itest=1;
try
    x1       = DesignVariable('value',10,'lowerBound',0,'upperBound',5);
    x2       = DesignVariable('value',10,'lowerBound',0,'upperBound',5);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2);
    Xobjfunc = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).x1-5*Tinput(i).x2; end','Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'},'Liostructure',true);
    Xcons1   = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).con1 = Tinput(i).x1 + Tinput(i).x2-5; end','Coutputnames',{'con1'},'Liostructure',true,...
        'Cinputnames',{'x1','x2'});
    Xoptprob = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
        'MinitialSolutions',randn(20,2),'Xconstraint',Xcons1);
    Cmess{itest}='Object initilized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object

itest = itest+1;
try
    Xobj = Cobyla; %#ok<*NASGU>
    display(Xobj);
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check display

itest = itest+1;
try
    Xobj = Cobyla('Nmaxiterations',2);
    display(Xobj);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4

itest = itest+1;
try
    Xobj = Cobyla('rho_ini',-2);
    display(Xobj);
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 5 Checking apply method

itest = itest+1;
try
    Xobj = Cobyla('Nmax',5);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    assert(strcmp(Xout.Sexitflag,'Maximum number of function evaluations reached'),...
    'Wrong exit flag')
    Cmess{itest}='Checking apply method';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 Check termination criteria

% in Xout, no of func. evaluations, evaluations of const are all zero ?

%% Finish the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='Cobyla';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Cobyla (' datestr(now) ')'])
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
