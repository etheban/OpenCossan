% Unit testing of Optimization Toolbox
	
function varargout=UnitTestEvolutionStrategy
	
Ntest=7;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% Initilize objects
itest=1;
try
x1       = DesignVariable('value',10);
x2       = DesignVariable('value',10);
Xin      = Input('XDesignVariable',x1,'XDesignVariable',x2);
Xobjfunc = ObjectiveFunction('Sscript','for j=1:length(Tinput),Toutput(j).out1=Tinput(j).x1-5*Tinput(j).x2;end','Cinputnames',{'x1','x2'},...
                             'Coutputnames',{'out1'},'Liostructure',true);
Xcons1   = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2','Coutputnames',{'con1'},'Liostructure',true,...
                      'Cinputnames',{'x1','x2'});
Xoptprob = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
                               'VinitialSolution',[5 5],'Xconstraint',Xcons1);
      Cmess{itest}='Object initilized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end                             
%% 2 Create an empty object

itest = itest+1;
try
    Xobj = EvolutionStrategy;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check display

itest = itest+1;
try
    Xobj = EvolutionStrategy('Nmaxiterations',2);
    display(Xobj);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 

itest = itest+1;
try
    Xobj = EvolutionStrategy('Nrho',-3);
    display(Xobj);
    Cmess{itest}='should fail - negative integer entered';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 5

itest = itest+1;
try
    Xobj = EvolutionStrategy('Srecombination','dummy');
    display(Xobj);
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 6

itest = itest+1;
try
    Xobj = EvolutionStrategy('Sselection','dummy');
    display(Xobj);
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end


%% 7 Checking apply method

itest = itest+1;
try
    Xobj = EvolutionStrategy('Nmax',30,'NmaxIterations',2);
    Xout = apply(Xobj,'XoptimizationProblem',Xoptprob,'Vsigma',[2 2]);
    display(Xout);
    Cmess{itest}='Checking apply method';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% TODO: add more tests
%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='EvolutionStrategy';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of EvolutionStrategy (' datestr(now) ')'])
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
