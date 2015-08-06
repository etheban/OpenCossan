function varargout = UnitTestProbabilisticModel
%UNITTESTEVALUATOR Summary of this function goes here
%   Detailed explanation goes here

% Test ProbabilisticModel
% Perallocate memory

Ntest=12;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

% defined required objects
Xvar1 = RandomVariable('Sdistribution','normal','mean',0,'std',1);
Xrvset = RandomVariableSet('CXrandomvariables',{Xvar1},'Cmembers',{'Xvar1'});
Xpar = Parameter('Mvalue',3);
Xinp = Input('Xrvset',Xrvset,'Xparameter',Xpar);
%generate some samples
Xinp = Xinp.sample('Nsamples',100);
Xmio = Mio('Sscript','for i=1:length(Tinput); Toutput(i).out1= Tinput(i).Xvar1;end;',...
    'Cinputnames',{'Xvar1'},'Coutputnames',{'out1'},'Liostructure',true);
Xpf  = PerformanceFunction('Scapacity','Xpar','Sdemand','out1','SoutputName','vg');
Xmodel = Model('Xevaluator',Evaluator('Xmio',Xmio),'Xinput',Xinp);
Xmc = MonteCarlo('Nsamples',100);
%% 1 Create an empty object
itest = itest+1;
try
    Xobj = ProbabilisticModel;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an object
itest = itest+1;
try
    Xobj = ProbabilisticModel('XPerformanceFunction',Xpf,'Xmodel',Xmodel);
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Create an object - GUI style
itest = itest+1;
try
    Xobj = ProbabilisticModel('CXPerformanceFunction',{Xpf},'CXmodel',{Xmodel});
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an object - GUI style';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Wrong objects passed to the constructor
itest = itest+1;
try
    Xobj = ProbabilisticModel('XPerformanceFunction',Xmio,'Xmodel',Xmodel);
    display(Xobj)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest = itest+1;
try
    Xobj = ProbabilisticModel('XPerformanceFunction',Xpf,'Xmodel',Xmio);
    display(Xobj)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 test apply method
itest = itest+1;
try
    Xout = Xobj.apply(Xinp);
    display(Xout)
    assert(isa(Xout,'SimulationData'),'Wrong output object class')
    Vtest(itest)=true;
    Cmess{itest}='apply method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest = itest+1;
try
    Xout = Xobj.apply(Xvar1);
    display(Xout)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test pf method
itest = itest+1;
try
    Xfprob = Xobj.pf(Xmc);
    display(Xfprob)
    assert(isa(Xfprob,'FailureProbability'),'Wrong output object class')
    Vtest(itest)=true;
    Cmess{itest}='pf method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
itest = itest+1;
try
    Xfprob = Xobj.pf(Xinp);
    display(Xfprob)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test designPointIdentification method
itest = itest+1;
try
    XprobModel = ProbabilisticModel('XPerformanceFunction',Xpf,'Xmodel',Xmodel);
    Xdp = XprobModel.designPointIdentification;
    assert(isa(Xdp,'DesignPoint'),'Wrong output object class')
    Vtest(itest)=true;
    Cmess{itest}='designPointIdentification method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11
itest = itest+1;
try
    Xdp = Xobj.designPointIdentification('Xoptimizer',Xmio,'MreferencePoint',0);
    display(Xdp)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 Check deterministic Analysis
itest = itest+1;
try
    Xobj = ProbabilisticModel('XPerformanceFunction',Xpf,'Xmodel',Xmodel);
    Xout = Xobj.deterministicAnalysis;
    display(Xout)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% finalize unit test
if nargout>0
% Export name of the UnitTest
varargout{1}='ProbabilisticModel';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the ProbabilisticModel (' datestr(now) ')'])
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



