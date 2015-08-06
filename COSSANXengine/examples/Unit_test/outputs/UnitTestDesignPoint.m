%% Unit Test for the DesignPoint
function varargout=UnitTestDesignPoint
% Test constructor
Ntest=7;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% The DesignPoint requires a ProbabilisticModel
% Contruct a probabilistic model
% Define Input parameters
% Random Variables
Xrv1    = RandomVariable('Sdistribution','normal', 'mean',1,'std',0.5); 
Xrv2    = RandomVariable('Sdistribution','normal', 'mean',-1,'std',2);

%  RandomVariableSets 
Xrvs1   = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1 Xrv2}); 

% Define Input
Xin     = Input;
Xin     = add(Xin,Xrvs1);

%  Construct Mio object
Xm      = Mio('Sdescription','normalized demand', ...
    'Spath','./',...
    'Sscript','Toutput.D=Tinput.RV1;',...
    'Liostructure',true,...
    'Lfunction',false,...
    'Cinputnames',{'Xrv1','Xrv2'},...
    'Coutputnames',{'D','C'});            

%  Construct evaluator
Xeval   = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');

%   Define Physical Model
Xmdl    = Model('Xevaluator',Xeval,'Xinput',Xin);

% Define Probabilistic Model
% Define Performance Function
Xperf   = PerformanceFunction('Scapacity','C','Sdemand','D','Soutputname','Vg');
% Construct a probmodel
Xpm     = ProbabilisticModel('Xmodel',Xmdl,'XperformanceFunction',Xperf);

%% Here we go

%% 1
% Constructor and display
itest=1;
try
    Xdp  = DesignPoint('Sdescription','My design point',...
        'XProbabilisticModel',Xpm,'VDesignPointPhysical',[0 0]);
    display(Xdp)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.message ' Error in: ' ME.stack(1).name];
end


%% 2
itest=itest+1;
try
    Xdp  = DesignPoint('Sdescription','My design point','VDesignPointPhysical',[0 0 0]);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xdp  = DesignPoint('Sdescription','My design point','PM',Xpm,'VDesignPointPhysical',[0 0 0]);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
% the constructor should check the number of values (i.e. inputs of the
% performance function = length(VDesignPointPhysical)
itest=itest+1;
try
    Xdp  = DesignPoint('XProbabilisticModel',Xpm,'VDesignPointPhysical',[0 0]);
    Vtest(itest)=true;
    Cmess{itest}='test create correctly';
catch ME
    Cmess{itest}=[ME.message ' in ' ME.stack(1).name];
end

%% 5
itest=itest+1;
try
    Xdp  = DesignPoint('XProbabilisticModel',Xpm,'VDesignPointStdNormal',[5 0]);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.message ' in ' ME.stack(1).name];
end

%% 6
itest=itest+1;
try
    Xdp=Xdp.set('VDesignPointPhysical',[3 1 ]);
    display(Xdp)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    Xdp=Xdp.set('VDesignPointPhysical',[3 1 1],'VDesignPointPhysical',[3]);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
OpenCossan.cossanDisp('--------------------------------------------------------------------')
OpenCossan.cossanDisp([' Unit Test of DesignPoint (' datestr(now) ')'])
OpenCossan.cossanDisp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

if nargout>0
% Export name of the UnitTest
varargout{1}='DesignPoint';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
