% Unit testing of Optimization Toolbox
	
function varargout=UnitTestSequentialQuadraticProgramming
	
Ntest=12;
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
        'VinitialSolution',[2 2],'Xconstraint',Xcons1);
    Cmess{itest}='Object initilized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming; %#ok<*NASGU>
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check display

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming; 
    display(Xsqp);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Wrong type of input

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('Sdescription',5);
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 5 Wrong type of input

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('Nmax',3.2);
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 6 Negative no of iterations

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('Nmax',-5);
    Cmess{itest}='should fail - negative integer entered';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 7 Negative no of iterations

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('NEvaluationsPerBatch',-5);
    Cmess{itest}='should fail - negative integer entered';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 8 Wrong type of input

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('LKeepFiles',3);
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 9 Wrong type of input

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('FiniteDifferencePerturbation','j');
    Cmess{itest}='should fail - wrong type of input';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 10  Negative no of iterations

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('NMaximumIterations',-3);
    Cmess{itest}='should fail - negative integer entered';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 11 Wrong type of input

itest = itest+1;
try
    Xsqp = SequentialQuadraticProgramming('ToleranceObjectiveFunction','f');
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 12 Checking apply method

itest = itest+1;
try
    Xout = apply(Xsqp,'XOptimizationProblem',Xoptprob);
    Cmess{itest}='Checking apply method';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='SequentialQuadraticProgramming';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of SequentialQuadraticProgramming (' datestr(now) ')'])
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
