%% Unit Test for the RBOProblem object
% BG

function varargout=UnitTestRBOProblem

% Test constructor
Ntest=9;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1
% construct empty object and display
itest=1;
try
    Xrbo  = RBOProblem;
    display(Xrbo)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
% Constructor with all required properties
itest=itest+1;
try
    % defined objects required for the ProbabilisticModel
    Xvar1 = RandomVariable('Sdistribution','normal','mean',0,'std',1);
    Xrvset = RandomVariableSet('CXrandomvariables',{Xvar1},'Cmembers',{'Xvar1'});
    Xpar = Parameter('value',0.5);
    Xinp = Input('Xrvset',Xrvset,'Xparameter',Xpar);
    Xmio = Mio('Sscript','for i=1:length(Tinput); Toutput(i).out1= Tinput(i).Xvar1;end;',...
        'Cinputnames',{'Xvar1'},'Coutputnames',{'out1'});
    Xpf  = PerformanceFunction('Scapacity','Xpar','Sdemand','out1','SoutputName','vg');
    Xmodel = Model('Xevaluator',Evaluator('Xmio',Xmio),'Xinput',Xinp);
    XprobModelA=ProbabilisticModel('Xmodel',Xmodel,'XperformanceFunction',Xpf);
    
    
    Xdv1 = DesignVariable('value',1,'lowerbound',0.5,'upperbound',1.5);
    Xdv2 = DesignVariable('value',0,'lowerbound',-0.5,'upperbound',0.5);
    Xin = Input('CSmembers',{'Xdv1' 'Xdv2'},'CXmember',{Xdv1 Xdv2});
    Xobj = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).pf; end', ...
                             'Cinputnames',{'pf'},'Coutputnames',{'out1'});
    
    Xrbo = RBOProblem('XprobabilisticModel',XprobModelA, ...
        'Xinput',Xin, ... 
        'XobjectiveFunction',Xobj,...
        'Xsimulator',MonteCarlo,...
        'SfailureProbabilityName','pf',... 
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'}); 
                     
    display(Xrbo)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
% This test shall fail since pf1 is not defined in the objective function
itest=itest+1;
try
    XmcA=MonteCarlo('Nsamples',1000,'Nbatches',1);
    Xrbo = RBOProblem('XprobabilisticModel',XprobModelA, ...
        'Xinput',Xin, ... 
        'Xsimulator',XmcA, ...
        'XobjectiveFunction',Xobj,...
        'SfailureProbabilityName','pf1',... 
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});           
    Cmess{itest} ='This test shall fail since pf1 is not defined in the objective function';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
% This test shall fail since the objective function is not passed
itest=itest+1;
try
    XmcA=MonteCarlo('Nsamples',1000,'Nbatches',1);
    Xrbo = RBOProblem('XprobabilisticModel',XprobModelA, ...
        'Xinput',Xin, ... 
        'Xsimulator',XmcA, ...
        'SfailureProbabilityName','pf',... 
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});           
    Cmess{itest} ='This test shall fail since the objective function is not passed';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
% Perform analysis
itest=itest+1;
try
    OpenCossan.resetRandomNumberGenerator(1000); %important for the check of the results
    XmcA=MonteCarlo('Nsamples',1000,'Nbatches',1);
    Xrbo = RBOProblem('XprobabilisticModel',XprobModelA, ...
        'Xinput',Xin, ... 
        'Xsimulator',XmcA, ...
        'XobjectiveFunction',Xobj,...
        'SfailureProbabilityName','pf',... 
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});           
    Xoptimum=Xrbo.optimize('Xoptimizer',Cobyla);
    display(Xoptimum)
    assert(abs(Xoptimum.XdesignVariable.Mdata(1,end)-0.5000)<1.e-2,'Wrong result for Xdv1');
    assert(abs(Xoptimum.XdesignVariable.Mdata(2,end)+0.5000)<1.e-2,'Wrong result for Xdv2');
    Vtest(itest)=true;
    Cmess{itest} ='Apply RBOProblem';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
% Perform analysis using global metamodel (neural network)
itest=itest+1;
try
    Xoptimum=Xrbo.optimizeGlobalMetaModel('Xoptimizer',Cobyla,...
                    'SmetaModelType','NeuralNetwork',...
                    'Xsimulator',DesignOfExperiments);
    display(Xoptimum)
    assert(abs(Xoptimum.XdesignVariable.Mdata(1,end)-0.5000)<1.e-2,'Wrong result for Xdv1');
    assert(abs(Xoptimum.XdesignVariable.Mdata(2,end)+0.5000)<1.e-2,'Wrong result for Xdv2');
    Vtest(itest)=true;
    Cmess{itest} ='Apply RBOProblem using global Neural Network';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7
% Perform analysis using local metamodel (neural network)
itest=itest+1;
try
    XmcB=MonteCarlo('Nsamples',100,'Nbatches',1);
    Xoptimum=Xrbo.optimizeLocalMetaModel('Vperturbation',[1 1], ...
                    'Xoptimizer',Cobyla,...
                    'SmetaModelType','NeuralNetwork',...
                    'Xsimulator',XmcB, ...
                    'VhiddenNodes',[4 2],'NmaxLocalRBOIterations',2);
    display(Xoptimum)
    assert(abs(Xoptimum.XdesignVariable.Mdata(1,end)-0.5000)<1.e-2,'Wrong result for Xdv1');
    assert(abs(Xoptimum.XdesignVariable.Mdata(2,end)+0.5000)<1.e-2,'Wrong result for Xdv2');
    Vtest(itest)=true;
    Cmess{itest} ='Apply RBOProblem using local Neural Network';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
% Perform analysis using global metamodel (response surface)
itest=itest+1;
try
    Xmc = MonteCarlo('Nsamples',20);
    Xoptimum=Xrbo.optimizeGlobalMetaModel('Xoptimizer',Cobyla,...
                    'SmetaModelType','ResponseSurface',...
                    'Xsimulator',Xmc, ...
                    'Stype','quadratic');
    display(Xoptimum)
    assert(abs(Xoptimum.XdesignVariable.Mdata(1,end)-0.5000)<1.e-2,'Wrong result for Xdv1');
    assert(abs(Xoptimum.XdesignVariable.Mdata(2,end)+0.5000)<1.e-2,'Wrong result for Xdv2');
    Vtest(itest)=true;
    Cmess{itest} ='Apply RBOProblem using global Neural Network';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
% Perform analysis using local metamodel (response surface)
itest=itest+1;
try
    Xoptimum=Xrbo.optimizeLocalMetaModel('Vperturbation',[0.3 0.4], ...
                    'Xoptimizer',Cobyla,...
                    'SmetaModelType','ResponseSurface',...
                    'Xsimulator',Xmc, ...
                    'Stype','quadratic');
    display(Xoptimum)
    assert(abs(Xoptimum.XdesignVariable.Mdata(1,end)-0.5000)<1.e-2,'Wrong result for Xdv1');
    assert(abs(Xoptimum.XdesignVariable.Mdata(2,end)+0.5000)<1.e-2,'Wrong result for Xdv2');
    Vtest(itest)=true;
    Cmess{itest} ='Apply RBOProblem using local Neural Network';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of RBOProblem (' datestr(now) ')'])
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
varargout{1}='RBOProblem';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
