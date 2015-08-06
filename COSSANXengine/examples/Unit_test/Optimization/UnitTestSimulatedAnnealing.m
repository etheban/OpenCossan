% Unit testing of Optimization Toolbox
	
function varargout=UnitTestSimulatedAnnealing
	
Ntest=6;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Predefine object

itest=1;
try
    x1       = DesignVariable('value',10);
    x2       = DesignVariable('value',10);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2);
    Xobjfunc = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).x1-5*Tinput(i).x2; end','Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'},'Liostructure',true);
    Xcons1   = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).con1 = Tinput(i).x1 + Tinput(i).x2-5; end','Coutputnames',{'con1'},'Liostructure',true,...
        'Cinputnames',{'x1','x2'});
    Xoptprob = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
        'MinitialSolutions',randn(1,2),'Xconstraint',Xcons1);
    Cmess{itest}='Object initilized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end                         

%% 2 Create an empty object

itest = itest+1;
try
    Xobj = SimulatedAnnealing; %#ok<*NASGU>
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check display

itest = itest+1;
try
    display(Xobj);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 invalid input

itest = itest+1;
try
    Xobj = SimulatedAnnealing('Ng',-3);
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 5 invalid property name

itest = itest+1;
try
    Xobj = SimulatedAnnealing('verbose','s');
    Cmess{itest}='invalid property name';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 testing apply method

itest = itest+1;
try
    Xobj = SimulatedAnnealing('Nmaxiterations',2); 
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob);
    Cmess{itest}='Checking apply method';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='SimulatedAnnealing';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the SimulatedAnnealing (' datestr(now) ')'])
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
