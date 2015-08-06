function varargout = UnitTestSystemReliability
%% Unit Test for the SystemReliability object

Ntest=11;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1. Prepare Objects required for testing SystemReliability
itest=1;
try
    Cmess{itest}='Preparing preliminary objects';
    % define necessary objects
    Xrv = RandomVariable('Sdistribution','normal','mean',0,'std',1);
    Xrvset = RandomVariableSet('Cmembers',{'Xrv'},'Cxrv',{Xrv},'Nrviid',3);
    Xpar = Parameter('value',3*sqrt(2));
    Xpf1 = PerformanceFunction('Scapacity','Xpar','Sdemand','out1','Soutputname','vg1');
    Xpf2 = PerformanceFunction('Scapacity','Xpar','Sdemand','out2','Soutputname','vg2');
    Xpf3 = PerformanceFunction('Scapacity','Xpar','Sdemand','out3','Soutputname','vg3');
    Xinput = Input('XRandomvariableset',Xrvset,'XParameter',Xpar);
    Xinput = Xinput.sample('Nsamples',100);
    Xmio = Mio('Cinputnames',{'Xrv_1' 'Xrv_2' 'Xrv_3'},...
        'Coutputnames',{'out1' 'out2' 'out3'},...
        'Liostructure',true,...
        'Sscript',['for i=1:length(Tinput);' ...
        'Toutput(i).out1 = Tinput(i).Xrv_1 + Tinput(i).Xrv_2;' ...
        'Toutput(i).out2 = Tinput(i).Xrv_2 + Tinput(i).Xrv_3;' ...
        'Toutput(i).out3 = Tinput(i).Xrv_1 + Tinput(i).Xrv_3;' ...
        'end']);
    Xmodel = Model('Xevaluator',Evaluator('Xmio',Xmio),'Xinput',Xinput);
    CnodeTypes={'Output','AND','Input','AND','Input','Input'};
    VnodeConnections = [0 1 2 2 4 4];
    CnodeNames={'Out','AND gate 1','Xpf1','AND gate 2','Xpf2','Xpf3'};
    Xfaulttree = FaultTree('CnodeTypes',CnodeTypes,...
        'CnodeNames',CnodeNames,...
        'VnodeConnections',VnodeConnections);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2. Create an empty object
itest=itest+1;
try
    Xout  = SystemReliability;
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Empty Object created';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3. Create an object, regular way
itest=itest+1;
try
    Xsys=SystemReliability('Cmembers',{'Xpf1';'Xpf2';'Xpf3'},...
        'XperformanceFunctions',[Xpf1 Xpf2 Xpf3],...
        'Xmodel',Xmodel,'XFaultTree',Xfaulttree);
    display(Xsys)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4. Constructor with wrong objects
itest=itest+1;
try
    Xsys=SystemReliability('Cmembers',{'Xpf1';'Xpf2';'Xpf3'},...
        'XperformanceFunctions',[Xpf1 Xpf2 Xpf3],...
        'XFaultTree',Xmio,'Xmodel',Xfaulttree);
    display(Xsys)
    Cmess{itest} = 'This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5.
itest=itest+1;
try
    Xsys=SystemReliability('Cmembers',{'Xpf1';'Xpf2';'Xpf3'},...
        'XperformanceFunctions',[Xrv Xrv Xrv],...
        'Xmodel',Xmodel,'XFaultTree',Xfaulttree);
    display(Xsys)
    Cmess{itest} = 'This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 6.
itest=itest+1;
try
    Xsys=SystemReliability('Cmembers',{'Xpf1';'Xpf2';'Xpf3'},...
        'XperformanceFunctions',[Xpf1 Xpf2 Xpf3],...
        'Xmodel',Xmodel,'XFaultTree',Xrvset);
    display(Xsys)
    Cmess{itest} = 'This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7.
%Constructor with wrong property names
itest=itest+1;
try
    Xsys=SystemReliability('Csomething',{'Xpf1';'Xpf2';'Xpf3'},...
        'XperformanceFunctions',[Xpf1 Xpf2 Xpf3],...
        'Xmodel',Xmodel,'XFaultTree',Xfaulttree);
    display(Xsys)
    Cmess{itest} = 'This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8. Constructor with missing mandatory fields
% no model
itest=itest+1;
try
    Xsys=SystemReliability('Cmembers',{'Xpf1';'Xpf2';'Xpf3'},...
        'XperformanceFunctions',[Xpf1 Xpf2 Xpf3],...
        'XFaultTree',Xfaulttree);
    display(Xsys)
    Cmess{itest} = 'This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9. no performance functions names
itest=itest+1;
try
    Xsys=SystemReliability('XperformanceFunctions',[Xpf1 Xpf2 Xpf3],...
        'Xmodel',Xmodel,'XFaultTree',Xfaulttree);
    display(Xsys)
    Cmess{itest} = 'This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10. no performance functions objects
itest=itest+1;
try
    Xsys=SystemReliability('Cmembers',{'Xpf1';'Xpf2';'Xpf3'},...       
        'Xmodel',Xmodel,'XFaultTree',Xfaulttree);
    display(Xsys)
    Cmess{itest} = 'This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11.test method apply
itest=itest+1;
try
    Xsys=SystemReliability('Cmembers',{'Xpf1';'Xpf2';'Xpf3'},...
         'CXperformanceFunctions',{Xpf1; Xpf2; Xpf3}, ...
        'Xmodel',Xmodel,'XFaultTree',Xfaulttree);
    Xmc=MonteCarlo;
    Xout = Xsys.pf('CXsimulation',{Xmc});
    asser(isa(Xout,'SimulationData'),'Wrong output object from method apply')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% finalize the test
if nargout>0
    % Export name of the UnitTest
    varargout{1}='SystemReliability';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of the SystemReliability (' datestr(now) ')'])
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
return
