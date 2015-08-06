%% Unit Test for the Object SimulationData
%%
function varargout=UnitTestSimulationData

% Test constructor
Tstruct=cell2struct(num2cell(rand(50,3)),{'pippo';'pluto';'minni'},2); 
Ntest=19;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% Constructor and display

%% 1
itest=1;
try
    Xobj=SimulationData('Tvalues',Tstruct);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xobj=SimulationData('Tvalues',Tstruct,'Cnames',{'pippo';'pluto';'minni';'test'} );   
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xobj = SimulationData('Sdescription','Unit Test','Cnames',...
        {'out1','out2','out3'},'Mvalues',[1.1 2.2 3.3; 4.4 5.5 6.6]);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xobj = SimulationData('Cnames',{'out2','out3'},...
        'Mvalues',[1.1 2.2 3.3; 4.4 5.5 6.6]);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 5
itest=itest+1;
try
    Xobj = SimulationData('Cnames',{'out1' 'out5' 'out2','out3'},...
        'Mvalues',[1.1 2.2 3.3; 4.4 5.5 6.6]);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
% Test operations with SimulationData object

itest=itest+1;
try
    Mvalues1 = [0.8007  1.4056  2.4109  1.1032  -0.3025  1.5000  2.5000
                1.4631  2.1828  3.6341  1.8230  -0.3599  1.5000  2.5000
                0.5203  3.2296  6.1990  1.8749  -1.3547  1.5000  2.5000
                -0.5559 3.9562  8.1904  1.7001  -2.2561  1.5000  2.5000
                1.9063  2.0082  3.0632  1.9573  -0.0509  1.5000  2.5000];
            
    Mvalues2 = [2.6066  1.1044  0.9056  1.8555  0.7511   1.5000  2.5000
                1.5002  1.1882  1.6263  1.3442  0.1560   1.5000  2.5000
                0.1972  2.1291  4.1596  1.1632  -0.9659  1.5000  2.5000
                1.2604  1.7863  2.9425  1.5234  -0.2629  1.5000  2.5000
                2.0261  1.7854  2.5577  1.9058  0.1204   1.5000  2.5000];

    Xout1 = SimulationData('Cnames',{'add1' 'sub1' 'linfunc1','Xrv1','Xrv2','Xpar1','Xpar2'},...
        'Mvalues',Mvalues1);
    Xout2 = SimulationData('Cnames',{'add1' 'sub1' 'linfunc1','Xrv1','Xrv2','Xpar1','Xpar2'},...
        'Mvalues',Mvalues2);
    Xobj = plus(Xout1,Xout2); 
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    Xobj = minus(Xout1,Xout2); 
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
itest=itest+1;
try
    Xobj = merge(Xout1,Xout2);    
    Vtest(itest)=true;
    display(Xobj)
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
itest=itest+1;
try
    Xobj = SimulationData('Tvalues',Tstruct);
    Xobj = addVariable(Xobj,'Cnames',{'sequence'},'Mvalues',(1:50)');
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
itest=itest+1;
try
    Xobj = SimulationData('Tvalues',Tstruct);
    Xobj = addVariable(Xobj,'Cnames',{'sequence'},'Mvalues',(1:5)');
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11
% Test with a addBatch
itest=itest+1;
try
    Xobj.getValues('Sname','pippo');
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12
itest=itest+1;
try
    Vout=Xobj.getValues;
    assert(size(Vout,1)==Xobj.Nsamples);
    assert(size(Vout,2)==length(Xobj.Cnames));
    Vtest(itest)=true;
catch ME
    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13
% test save and load
itest=itest+1;
try
    Xout7 = SimulationData('Sdescription','new output', ...
        'Cnames',{'a','b','c'},'Mvalues',randn(5,3));
    Xout1.save('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationData1'));
    Xout2.save('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationData2'));
    Xout7.save('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationData7'));
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 14
% test save and load
clear Xout1 Xout2 Xout7
itest=itest+1;
try
    Xout1=SimulationData.load('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationData1'));
    Xout2=SimulationData.load('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationData2'));
    Xout7=SimulationData.load('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationData7'));
    display(Xout1)
    display(Xout2)
    display(Xout7)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15
% test save and load
itest=itest+1;
try
    Xout1=SimulationData.load('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationData1'),'Cnames',{'Xpar1'});
    display(Xout1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

delete(fullfile(OpenCossan.getCossanWorkingPath,'SimulationData1'))
delete(fullfile(OpenCossan.getCossanWorkingPath,'SimulationData2'))
delete(fullfile(OpenCossan.getCossanWorkingPath,'SimulationData7'))


%% 16 
% Test simulationOutput with DataSeries
itest=itest+1;
try
    T(2).ts=Dataseries;
    T(2).a=[1 5];
    T(1).ts=Dataseries;
    T(1).a=[3 2];
    T(3).ts=Dataseries;
    T(3).a=[1 1];
    Xout1=SimulationData('Tvalues',T);
    display(Xout1)
    Vtest(itest)=true;
    Cmess{itest}='Test SimulationData with Dataseries';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17 
% Test save with DataSeries
itest=itest+1;
try
    T(2).ts=Dataseries;
    T(2).a=[1 5];
    T(1).ts=Dataseries;
    T(1).a=[3 2];
    T(3).ts=Dataseries;
    T(3).a=[1 1];
    Xout1=SimulationData('Tvalues',T);
    display(Xout1)
    Xout1.save('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationDataDS1'));
    delete(fullfile(OpenCossan.getCossanWorkingPath,'SimulationDataDS1'))
    Vtest(itest)=true;
    Cmess{itest}='Test save with Dataseries';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 18 
% Test load with DataSeries
itest=itest+1;
try
    T(2).ts=Dataseries;
    T(2).a=[1 5];
    T(1).ts=Dataseries;
    T(1).a=[3 2];
    T(3).ts=Dataseries;
    T(3).a=[1 1];
    Xout1=SimulationData('Tvalues',T);
    display(Xout1)
    Xout1.save('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationDataDS'));
    Xout2=SimulationData.load('SfileName',fullfile(OpenCossan.getCossanWorkingPath,'SimulationDataDS'));
    delete(fullfile(OpenCossan.getCossanWorkingPath,'SimulationDataDS'))
    Vtest(itest)=true;
    Cmess{itest}='Test load with Dataseries';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 19 
% Test simulationOutput with Parameters
itest=itest+1;
try
    Xrv=RandomVariable('Sdistribution','normal','mean',0,'std',1);
    Xrvset=RandomVariableSet('Cmembers',{'Xrv'},'CXrv',{Xrv});
    Xpar=Parameter('Cvalue',{45});
    Xinput=Input;
    Xinput=Xinput.add(Xpar);
    Xinput=Xinput.add(Xrvset);
    Xinput=Xinput.sample('Nsamples',10);
    Xev=Evaluator;
    Xmdl=Model('Xinput',Xinput,'Xevaluator',Xev);
    Xout1=Xmdl.apply(Xinput);
    display(Xout1)
    Xout1.getValues('Sname','Xpar')
    Vtest(itest)=true;
    Cmess{itest}='Test get parameter from simulation output';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
OpenCossan.cossanDisp('--------------------------------------------------------------------')
OpenCossan.cossanDisp([' Unit Test of SimulationData (' datestr(now) ')'])
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
varargout{1}='SimulationData';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
