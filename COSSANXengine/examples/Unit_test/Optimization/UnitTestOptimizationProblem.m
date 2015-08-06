% Unit testing of Optimization Toolbox
	
function varargout=UnitTestOptimizationProblem
	
Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Predefine object

itest=1;
try
    x1       = DesignVariable('value',10);
    x2       = DesignVariable('value',10);
    x3       = DesignVariable('value',10);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2);
    Xin2      = Input('Xdesignvariable',x1,'Xdesignvariable',x2,'Xdesignvariable',x3);
    Xobjfunc = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).x1-5*Tinput(i).x2; end','Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'},'Liostructure',true);
    Xobjfunc2 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out2=Tinput(i).x1-5*Tinput(i).x3; end','Cinputnames',{'x1' 'x3'},...
        'Coutputnames',{'out2'},'Liostructure',true);
    Xcons1   = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).con1 = Tinput(i).x1 + Tinput(i).x2-5; end','Coutputnames',{'con1'},'Liostructure',true,...
        'Cinputnames',{'x1','x2'});
    Xcons2   = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).con2 = Tinput(i).x2-5; end','Coutputnames',{'con2'},'Liostructure',true,...
        'Cinputnames',{'x2'});
    Xoptprob = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
        'VinitialSolution',[2 2],'Xconstraint',Xcons1);
    
    Xrv1  = RandomVariable('Sdistribution','normal','mean',100,'std',15);
    Xrv2  = RandomVariable('Sdistribution','normal','mean',100,'std',15);
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2});
    Xfun1 = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    Xpar  = Parameter('value',5);
    DV = DesignVariable('value',30,'minvalue',10,'maxvalue',50);
    Xinput2   = Input('CSmembers',{'Xrvs' 'Xpar' 'Xfun1' 'x1'},'CXmembers',{Xrvs Xpar Xfun1 x1});

    Xmodel=Model('Xinput',Xinput2','Xevaluator',Evaluator);
    Cmess{itest}='Object initilized correctly';
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object

itest = itest+1;
try
    Xobj = OptimizationProblem; %#ok<*NASGU>
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check display

itest = itest+1;
try
    %TODO: test display also for not empty object
    Xobj = OptimizationProblem;
    display(Xobj);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 No Xinput provided 

itest = itest+1;
try       
    Xobj = OptimizationProblem('XobjectiveFunction',Xobjfunc);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 No DesignVariables provided

itest = itest+1;
try       
    XinNODV=Input; % Input without DesignVariable
    Xobj = OptimizationProblem('Xinput',XinNODV,'XobjectiveFunction',Xobjfunc);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 No constraints defined

itest = itest+1;
try       
    Xobj = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc);
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='According to the Ref. Manual it should not be possible to create the object without defining constraints';
catch ME
    Vtest(itest)=true;
end

%% 7 Inconsistent length of Vlowerbound
% TODO: to be improved
itest = itest+1;
try          
    Xobj = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
                              'Xconstraint',Xcons1);
Vtest(itest)=true;
   catch ME
       Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 Create an Optimization Problem Object with all input parameters

itest = itest+1;
try       
    Xobj = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
                               'VinitialSolution',[2 2],'Xconstraint',Xcons1);
    Cmess{itest}='Create an Optimization Problem Object';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 testing method optimize

itest = itest+1;
try       
    Xopt         = SequentialQuadraticProgramming; 
    Xoptimum     = Xobj.optimize('Xoptimizer',Xopt);
    Cmess{itest} = 'testing method optimize';
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 testing dynamic field 

itest = itest+1;
try       
    Xoptprob = OptimizationProblem('Xinput',Xin2,'CXobjectiveFunctions',{Xobjfunc Xobjfunc2},...
        'VinitialSolution',[2 2 3],'CXconstraint',{Xcons1 Xcons2},'Xmodel',Xmodel);
    assert(all(strcmp(Xoptprob.CobjectiveFunctionNames,{'out1','out2'})),...
        'CossanX:Unittest','wrong objective function output names')
    Cmess{itest} = 'testing method optimize';
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 testing dynamic field 
itest = itest+1;
try       
    Xoptprob = OptimizationProblem('Xinput',Xin2,'CXobjectiveFunctions',{Xobjfunc Xobjfunc2},...
        'VinitialSolution',[2 2 3],'CXconstraint',{Xcons1 Xcons2},'Xmodel',Xmodel);
    assert(all(strcmp(Xoptprob.CconstraintsNames,{'con1','con2'})),...
        'CossanX:Unittest','wrong constraints output names')
    Cmess{itest} = 'testing method optimize';
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 12 testing dynamic field 
itest = itest+1;
try       
    Xoptprob = OptimizationProblem('Xinput',Xin2,'CXobjectiveFunctions',{Xobjfunc Xobjfunc2},...
        'VinitialSolution',[2 2 3],'CXconstraint',{Xcons1 Xcons2},'Xmodel',Xmodel);
    assert(all(strcmp(Xoptprob.Cinputnames,{ 'Xfun1'    'Xpar'    'Xrv1'    'Xrv2'    'x1'    'x2'    'x3'})),...
        'CossanX:Unittest','wrong input names')
    Cmess{itest} = 'testing method optimize';
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 testing dynamic field 
itest = itest+1;
try       
    Xoptprob = OptimizationProblem('Xinput',Xin2,'CXobjectiveFunctions',{Xobjfunc Xobjfunc2},...
        'VinitialSolution',[2 2 3],'CXconstraint',{Xcons1 Xcons2},'Xmodel',Xmodel);
    assert(all(strcmp(Xoptprob.Coutputnames,{  'out1'    'out2'    'con1'    'con2'})),...
        'CossanX:Unittest','wrong output names')
    Cmess{itest} = 'testing method optimize';
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='OptimizationProblem';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of OptimizationProblem (' datestr(now) ')'])
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
