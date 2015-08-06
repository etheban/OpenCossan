%% Unit Test for the Optimum object
% EP

function varargout=UnitTestOptimum

% Test constructor
Ntest=19;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

% The DesignPoint requires a ProbabilisticModel
% TODO: Please test the plot Methods

%% 1
% Constructor and display
itest=1;
try
    Xout  = Optimum('Sdescription','my output object');
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2
itest=itest+1;
try

    DS = Dataseries;
    DS(2) = Dataseries;
    Xout  = Optimum('Xobjectivefunctiondataseries',DS);
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    DS = Dataseries;
    Xout  = Optimum('Cdesignvariablenames',{'dv1'},'Xdesignvariabledataseries',DS);
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 4
% the constructor should check the number of values (i.e. inputs of the
% performance function = length(VDesignPointPhysical)
itest=itest+1;
try
    DS = Dataseries;
    Xout  = Optimum('Xconstrainsdataseries',DS);
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 5

itest=itest+1;
try
    Xout  = Optimum('Sexitflag','test');
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
% This should fail: it is not allowed to set the number of function
% evaluations
try
    Xout  = Optimum('NEvaluationsObjectiveFunction',1,'NEvaluationsConstraints',134);
    display(Xout)
    Cmess{itest}='This values are computed automatically and cannot be set by the user';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end


%% 7
itest=itest+1;
try
    Xout  = Optimum('totalTime',0.1);
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 8
% this should fail since the Xop and Xga are inverted
itest=itest+1;
try
    Xga=GeneticAlgorithms;
    Xop=OptimizationProblem;
    Xout  = Optimum('totalTime',0.1,'Xoptimizer',Xop,'Xoptimizationproblem',Xga);
    display(Xout)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
itest=itest+1;
try
    Xga=GeneticAlgorithms;
    Xop=OptimizationProblem;
    Xout  = Optimum('Xoptimizer',Xga,'Xoptimizationproblem',Xop);
    Vtest(itest)=true;
    display(Xout)    
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 Check number of fields
itest=itest+1;
try
    Xout  = Optimum('totalTime',0.1);
    Xga=GeneticAlgorithms;
    Xop=OptimizationProblem;
    Xout2  = Optimum('Xoptimizer',Xga,'Xoptimizationproblem',Xop);
    assert(isa( Xout.CconstraintsNames,'cell'),'Wrong field CconstraintsName')
    assert(isa( Xout.CobjectiveFunctionNames,'cell'),'Wrong field CobjectiveFunctionNames')
    assert(isa( Xout2.CobjectiveFunctionNames,'cell'),'Wrong field CobjectiveFunctionNames')
    assert(isa( Xout2.CconstraintsNames,'cell'),'Wrong field CconstraintsName')
    Vtest(itest)=true;
    Cmess{itest}='Check number of fields';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11
itest=itest+1;
try
    Xds1 = Dataseries('Vindex',(1:100)','Mdata',rand(1,100),'Sindexname','DesignVariable','Sindexunit','Iterations');
    Xds1(2) = Dataseries('Vindex',(1:100)','Mdata',rand(1,100),'Sindexname','DesignVariable','Sindexunit','Iterations');
    Xout  = Optimum('Cdesignvariablenames', {'DV1' 'DV2'},'Xdesignvariabledataseries',Xds1);
    Vtest(itest)=true;
    display(Xout)    
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12
itest=itest+1;
try
    Xds = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','ObjectiveFucntion','Sindexunit','Iterations');
    Xds(2) = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','ObjectiveFucntion','Sindexunit','Iterations');
    Xout  = Optimum('Cdesignvariablenames', {'DV1' 'DV2'},'XobjectiveFunctionDataseries',Xds);
    Vtest(itest)=true;
    display(Xout)    
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13
itest=itest+1;
try
    Xds1 = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','ObjectiveFucntion','Sindexunit','Iterations');
    Xds1(2) = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','ObjectiveFucntion','Sindexunit','Iterations');
   
    Xds2 = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','Constraint','Sindexunit','Iterations');
    Xds2(2) = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','Constraint','Sindexunit','Iterations');
    Xds2(3) = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','Constraint','Sindexunit','Iterations');

    x1       = DesignVariable('value',10,'lowerBound',0,'upperBound',5);
    x2       = DesignVariable('value',10,'lowerBound',0,'upperBound',5);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2);
    Xobjfunc1 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).x1-5*Tinput(i).x2; end','Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'},'Liostructure',true);
    Xobjfunc2 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out2=Tinput(i).x1-5*Tinput(i).x2; end','Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out2'},'Liostructure',true);
    Xcons1   = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).con1 = Tinput(i).x1 + Tinput(i).x2-5; end','Coutputnames',{'con1'},'Liostructure',true,...
        'Cinputnames',{'x1','x2'});
    Xcons2   = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).con2 = Tinput(i).x1 + Tinput(i).x2-5; end','Coutputnames',{'con2'},'Liostructure',true,...
        'Cinputnames',{'x1','x2'});
    Xcons3   = Constraint('Sscript','for i=1:length(Tinput); Toutput(i).con3 = Tinput(i).x1 + Tinput(i).x2-5; end','Coutputnames',{'con3'},'Liostructure',true,...
        'Cinputnames',{'x1','x2'});
    Xoptprob = OptimizationProblem('Xinput',Xin,'CXobjectiveFunctions',{Xobjfunc1 Xobjfunc2},...
            'CXconstraint',{Xcons1 Xcons2 Xcons3});   
    
    Xout13  = Optimum('Cdesignvariablenames', {'DV1' 'DV2'},'XobjectiveFunctionDataseries',Xds1, ...
        'XconstrainsDataseries',Xds2,'XoptimizationProblem',Xoptprob);
    Vtest(itest)=true;
    display(Xout13)    
    Cmess{itest}='Object created and disply test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 merge Optimum objects

itest=itest+1;
try  
    Xout14 = Xout13.merge(Xout13);
    assert(Xout14.NevaluationsObjectiveFunction==20,'Optimum objects not merged correctly');
    Vtest(itest)=true;
    Cmess{itest}='Objects merged correctly';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15 merge objects with empty fields for constraints 

itest=itest+1;
try
    
    Xoptprob = OptimizationProblem('Xinput',Xin,'CXobjectiveFunctions',{Xobjfunc1 Xobjfunc2});   
    Xout15  = Optimum('Cdesignvariablenames', {'DV1' 'DV2'},'XobjectiveFunctionDataseries',Xds1, ...
              'XoptimizationProblem',Xoptprob);
    Xout15 = Xout15.merge(Xout15);
    assert(Xout15.NevaluationsObjectiveFunction==20,'Optimum objects not merged correctly');
    Vtest(itest)=true;
    Cmess{itest}='Objects merged correctly';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 16 this test shall fail since the two Optimum objects contain different number of objective functions
itest=itest+1;
try
    Xds1 = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','ObjectiveFucntion','Sindexunit','Iterations');
    Xoptprob = OptimizationProblem('Xinput',Xin,'CXobjectiveFunctions',{Xobjfunc1});   
    Xout16  = Optimum('Cdesignvariablenames', {'DV1' 'DV2'},'XobjectiveFunctionDataseries',Xds1, ...
              'XoptimizationProblem',Xoptprob);
    
    Xout16 = Xout15.merge(Xout16);
    Cmess{itest} = 'This test shall fail since the two Optimum objects contain different number of objective functions';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17 this test shall fail since the two Optimum objects contain different design variables
itest=itest+1;
try
    Xds1 = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','ObjectiveFucntion','Sindexunit','Iterations');
    Xds1(2) = Dataseries('Vindex',(1:10)','Mdata',rand(1,10),'Sindexname','ObjectiveFucntion','Sindexunit','Iterations');

    Xobjfunc1 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).x1-5*Tinput(i).x1; end','Cinputnames',{'x1'},...
        'Coutputnames',{'out1'},'Liostructure',true);
    Xobjfunc2 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out2=Tinput(i).x1-5*Tinput(i).x1; end','Cinputnames',{'x1'},...
        'Coutputnames',{'out2'},'Liostructure',true);
    Xin = Input('Xdesignvariable',x1);
    Xoptprob = OptimizationProblem('Xinput',Xin,'CXobjectiveFunctions',{Xobjfunc1 Xobjfunc2});   
    
    Xout17  = Optimum('Cdesignvariablenames', {'DV1'},'XobjectiveFunctionDataseries',Xds1, ...
              'XoptimizationProblem',Xoptprob);
    
    Xout17 = Xout15.merge(Xout17);
    Cmess{itest} = 'This test shall fail since the two Optimum objects contain different design variables';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 18
itest=itest+1;
try
    Xsqp = SequentialQuadraticProgramming;
    Xop=OptimizationProblem;
    Xout  = Optimum('Xoptimizer',Xsqp,'Xoptimizationproblem',Xop);
    Vtest(itest)=true;
    display(Xout)  
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 19
itest=itest+1;
Ttest.a=1;
Ttest.b=2;
% it should fail. It can not be possible that any random structure can be
% used
try
    Xsqp = SequentialQuadraticProgramming;
    Xop=OptimizationProblem;
    Xout  = Optimum('Xoptimizer',Xsqp,'Xoptimizationproblem',Xop,'TConstraintEvaluations',Ttest);
    display(Xout)  
    Cmess{itest}='it should fail. It can not be possible that any random structure can be  used';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of Optimum (' datestr(now) ')'])
disp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

if nargout>0
% Export name of the UnitTest
varargout{1}='Optimum';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
