%% Unit Test for the CutSet object
function varargout=UnitTestCutSet

% Total numeber of tests
Ntest=10;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

Xrv1    = RandomVariable('Sdistribution','normal', 'mean',1,'std',0.5);
Xrv2    = RandomVariable('Sdistribution','normal', 'mean',-1,'std',2);
%  RandomVariableSets
Xrvs1   = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1 Xrv2});
% Define Input
Xin     = Input('CXmembers',{Xrvs1},'CSmembers',{'Xrvs1'});
Xdp  = DesignPoint('Sdescription','My design point',...
    'VDesignPointPhysical',[0 0],'Xinput',Xin);


%% 1
% Empty object
itest=1;
try
    Xcs  = CutSet;
    display(Xcs)
    Vtest(itest)=true;
    Cmess{itest}='Empty Object created';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2 Constructor and display
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFaultTree',FaultTree,'VcutsetIndex',[1 2]);
    display(Xcs)
    Vtest(itest)=true;
    Cmess{itest}='Object created';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 3
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XDesignPoint',Xdp,'VcutsetIndex',[1 2]);
    display(Xcs)
    Vtest(itest)=true;
    Cmess{itest}='Object created';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFailureProbability',FailureProbability,'VcutsetIndex',[1 2]);
    display(Xcs)
    Vtest(itest)=true;
    Cmess{itest}='Object created';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFailureProbability',FailureProbability, ...
        'XDesignPoint',Xdp,'XFaultTree',FaultTree,'VcutsetIndex',[1 2]);
    display(Xcs)
    Vtest(itest)=true;
    Cmess{itest}='Object created';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFailureProbability',FailureProbability, ...
        'XDesignPoint',Xdp,'XFaultTree',FaultTree);
    display(Xcs)
catch ME
    % This should fail
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7
% this test shoud fail
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFailureProbability',FailureProbability, ...
        'XDesignPoint',FaultTree,'XFaultTree',FaultTree);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
% this test shoud fail
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFailureProbability',FailureProbability, ...
        'XDesignPoint',Xdp,'XFaultTree',DesignPoint);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
% this test shoud fail
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFailureProbability',FaultTree, ...
        'XDesignPoint',Xdp,'XFaultTree',FaultTree);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 10
itest=itest+1;
try
    Xcs  = CutSet('Sdescription','My cutset','XFailureProbability',FailureProbability, ...
        'XDesignPoint',Xdp,'XFaultTree',FaultTree,'Mcutset',[0 1 0 1; 0 0 0 1]);
    display(Xcs)
    Vtest(itest)=true;
    Cmess{itest}='Object created';
catch ME
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
    varargout{1}='CutSet';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
end
