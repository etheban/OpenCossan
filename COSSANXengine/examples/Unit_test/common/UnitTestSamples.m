%% Unit Test of the Samples object

function varargout = UnitTestSamples

Ntest=41;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

% Create objects required to test Samples object
Xrv1    = RandomVariable('Sdistribution','weibull','Cpar',{'par1',363.28;'par2',62.11});
Xrv2    = RandomVariable('Sdistribution','gumbel-i','mean',480,'std',48);
Xrv4    = RandomVariable('Sdistribution','lognormal','mean',50,'std',15);
Xrv3    = RandomVariable('Sdistribution','normal','mean',100,'std',20);
Xrv5    = RandomVariable('Sdistribution','normal','mean',10,'std',10);
Xrv6    = RandomVariable('Sdistribution','normal','mean',100,'std',1);

Xdv1    = DesignVariable('value',31);
Xdv2    = DesignVariable('value',0.54,'lowerBound',0.02,'upperBound',1);
Xdv3    = DesignVariable('Vsupport',[1 4 6 7]);

Xrvs    = RandomVariableSet('Cmembers',{'Xrv1','Xrv2','Xrv3','Xrv4'}...
    ,'Xrv',[Xrv1,Xrv2,Xrv3,Xrv4]);

Xrvs2    = RandomVariableSet('Cmembers',{'Xrv51','Xrv6'}...
    ,'Xrv',[Xrv5,Xrv6]);

time1   = 0:0.1:99;
Xcovfun =CovarianceFunction('Sdescription','covariance function', ...
    'Sscript','sigma = 1;b = 0.5;for i=1:length(Tinput),t1  = Tinput(i).t1;t2  = Tinput(i).t2;Toutput(i).fcov  = sigma^2*exp(-1/b*abs(t2-t1));end', ...
    'Lfunction',false, ...
    'Liostructure', true, ...
    'Liomatrix', false, ...
    'Coutputnames',{'fcov'},...
    'Cinputnames',{'t1','t2'});

Xsp    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xcovfun,'Mcoord',time1,'Lhomogeneous',true);
Xsp    = KL_terms(Xsp,'NKL_terms',30,'LcovarianceAssemble',false);

Xin     = Input;
Xin     = add(Xin,Xrvs);

Xin4     = add(Xin,Xdv1);
Xin4     = add(Xin4,Xdv2);
Xin4     = add(Xin4,Xdv3);

Xin2    = add(Xin,Xrvs2);

Xin3    = add(Xin,Xsp);



%% Test Constructors

%% 1
itest=1;
try
    Xs=Samples('Xrvset',Xrvs,'MsamplesHyperCube',rand(100,4));
    display(Xs)
    Vtest(itest)=true;
    Cmess{itest}='Constructor';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xs=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(100,4));
    display(Xs)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xs= Samples('Xinput',Xin,'MsamplesHyperCube',[0.1 0.1 0 0.1]);
    display(Xs)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xs= Samples('Xinput',Xin,'MsamplesPhysicalSpace',rand(100,4));
    display(Xs)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    % The constructor should faild becouse the number on colums of the
    % sampes are not equal to the number of variables of the
    % RandomVariableSet
    Xs= Samples('Xinput',Xin,'MsamplesHyperCube',[0.1 0.1]);
    display(Xs)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    % The constructor should faild becouse the number on colums of the
    % sampes are not equal to the number of variables of the
    % RandomVariableSet
    Xs=Samples('Xrvset',Xrvs,'MsamplesPhysicalSpace',rand(100,8));
    display(Xs)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    % The constructor should faild becouse the number on colums of the
    % sampes are not equal to the number of variables of the
    % RandomVariableSet
    Xs=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(100,8));
    display(Xs)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 - Construct Samples with more than 1 RandomVariableSet
itest=itest+1;
try
    Xs=Samples('Cxrvset',{Xrvs Xrvs2},'MsamplesStandardNormalSpace',rand(100,6));
    display(Xs)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

itest=itest+1;
try
    Xs=Samples('Xrvset',[Xrvs Xrvs2],'MsamplesStandardNormalSpace',rand(100,8));
    display(Xs)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
itest=itest+1;
try
    Xs=Samples('Xinput',Xin2,'MsamplesStandardNormalSpace',rand(100,6));
    display(Xs)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test method - add
%% 11
itest=itest+1;
try
    Xs1=Samples('Xinput',Xin,'MsamplesStandardNormalSpace',rand(20,4));
    Xs2=Samples('Xinput',Xin,'MsamplesStandardNormalSpace',rand(10,4));
    Xstot=Xs1.add('Xsamples',Xs2);
    display(Xstot)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xs2=Samples('Xrvset',Xrvs2,'MsamplesStandardNormalSpace',rand(20,2));
    Xstot=Xs1.add('Xsamples',Xs2);
    display(Xstot)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xs2=Samples('Xrvset',Xrvs2,'MsamplesStandardNormalSpace',rand(10,2));
    Xstot=Xs1.add('Xsamples',Xs2);
    display(Xstot)
    % This should fail since the number of samples differ from the 2
    % Samples object
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xstot=Xs1.add('MsamplesStandardNormalSpace',rand(10,4));
    display(Xstot)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xstot=Xs1.add('MsamplesStandardNormalSpace',rand(10,2));
    display(Xstot)
    % This should fail since the number of samples differ from the 2
    % Samples object
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 16
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xstot=Xs1.add('MsamplesHyperCube',rand(10,4));
    display(Xstot)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xstot=Xs1.add('MsamplesHyperCube',rand(10,2));
    display(Xstot)
    % This should fail since the number of samples differ from the 2
    % Samples object
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 18
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xstot=Xs1.add('MsamplesPhysicalSpace',rand(10,4));
    display(Xstot)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 19
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xstot=Xs1.add('MsamplesPhysicalSpace',rand(10,2));
    display(Xstot)
    % This should fail since the number of samples differ from the 2
    % Samples object
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 20: add a Dataseries object to the Samples object
itest=itest+1;
try
    Xs = sample(Xsp,'Nsamples',10);
    Xds = Xs.Xdataseries;
    Xs1 = Samples('XstochasticProcess',Xsp,'Xdataseries',Xds);
    display(Xs1)
    
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 21: chop samples from the Samples object that contains a Dataseries object
itest=itest+1;
try
    Xs = sample(Xsp,'Nsamples',10);
    Xds = Xs.Xdataseries;
    Xs1 = Samples('XstochasticProcess',Xsp,'Xdataseries',Xds);
    Xs1=Xs1.chop('Vchopsamples',[2 7]);
    display(Xs1)
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 22: add samples to the Samples object that contains a Dataseries object
itest=itest+1;
try
    Xs = sample(Xsp,'Nsamples',10);
    Xds = Xs.Xdataseries;
    Xs1 = Samples('XstochasticProcess',Xsp,'Xdataseries',Xds);
    Xs = sample(Xsp,'Nsamples',15);
    Xs1=Xs1.add('Xsamples',Xs);
    display(Xs1)
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 23: add an Input object, that contains a Dataseries object and rv's, to
%% the Samples object
itest=itest+1;
try
    Xs = sample(Xsp,'Nsamples',10);
    Xds = Xs.Xdataseries;
    Xs1 = Samples('Xinput',Xin3,'Xdataseries',Xds,'MsamplesStandardNormalSpace',randn(10,4));
    display(Xs1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 24: chop samples from the Samples object that contains a Dataseries
%% object and rv's
itest=itest+1;
try
    Xs = sample(Xsp,'Nsamples',10);
    Xds = Xs.Xdataseries;
    Xs1 = Samples('Xinput',Xin3,'Xdataseries',Xds,'MsamplesStandardNormalSpace',randn(10,4));
    Xs1=Xs1.chop('Vchopsamples',[2 7]);
    display(Xs1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 25: add samples to the Samples object that contains a Dataseries object
%% and rv's
itest=itest+1;
try
    Xs = sample(Xsp,'Nsamples',10);
    Xds = Xs.Xdataseries;
    Xs1 = Samples('Xinput',Xin3,'Xdataseries',Xds,'MsamplesStandardNormalSpace',randn(10,4));
    Xin3 = sample(Xin3,'Nsamples',15);
    Xs1=Xs1.add('Xsamples',Xin3.Xsamples);
    display(Xs1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 26
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xs2=Xs1.chop('Vchopsamples',[2 7]);
    display(Xs2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 27
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xs2=Xs1.chop('Vchopsamples',[2 21]);
    display(Xs2)
    % This should fail since the number of samples are only 20
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Test [Vcf Vval]  = cumulativeFrequencies(Xs,'Starget','RV1')
%% 28
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Vcf=Xs1.cumulativeFrequencies('Starget','Xrv1');
    display(Vcf)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 29
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~, V2]=Xs1.cumulativeFrequencies('Starget','Xrv1');
    display(V2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 30
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~, V2]=Xs1.cumulativeFrequencies;
    display(V2)
    % This should fail because the Starget is not define
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test [Vrf, Vval] = relativeFrequencies(Xs,'Starget','RV1')
%% 31
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Vcf=Xs1.relativeFrequencies('Starget','Xrv1');
    display(Vcf)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 32
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~, V2]=Xs1.relativeFrequencies('Starget','Xrv1');
    display(V2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 33
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~, V2]=Xs1.relativeFrequencies;
    display(V2)
catch ME
    % This should fail because the Starget is not define
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% TEST SORT Xs1 = sort(Xs,'Starget','RV1','Stype','descend')

%% 34
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    Xsorted=Xs1.sort('Starget','Xrv1','Stype','descend');
    display(Xsorted)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 35
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~, Vsort]=Xs1.sort('Starget','Xrv1','Stype','ascend');
    display(Vsort)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 36
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~, ~, V2]=Xs1.sort('Starget','Xrv1','Stype','ascend');
    display(V2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 37
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~, ~, V2]=Xs1.sort('Stype','ascend');
    display(V2)
catch ME
    % This should fail because the field Starget has not been defined.
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 38
itest=itest+1;
try
    Xs1=Samples('Xrvset',Xrvs,'MsamplesStandardNormalSpace',rand(20,4));
    [~,~, V2]=Xs1.sort('Stype','ascend','Vindex',[1 2 20],'Starget','Xrv1');
    display(V2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 39 Test DesignOfExperiment
itest=itest+1;
try
    % This should work
    Xs=Samples('Xinput',Xin4,'MsamplesStandardNormalSpace',rand(20,4),'MsamplesDoeDesignVariables',rand(20,3));
    display(Xs)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 40
itest=itest+1;
try
    Xin4=Xin4.sample('Nsamples',10);
    Vs=size(Xin4.Xsamples.MdoeDesignVariables);
    assert(all(Vs==[10 3]))
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 41 Test Set DOE
itest=itest+1;
try
    MsampleDoe(:,1)=Xdv1.sample('Nsamples',10,'perturbation',2);
    MsampleDoe(:,2)=Xdv2.sample('Nsamples',10,'perturbation',2);
    MsampleDoe(:,3)=Xdv3.sample('Nsamples',10);
    
    Xs=Samples('Xinput',Xin4,'MsamplesStandardNormalSpace',rand(20,4),'MsamplesDoeDesignVariables',MsampleDoe);
    Cmess{itest}='This should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 42 Test Set DOE
itest=itest+1;
try
    MsampleDoe(:,1)=Xdv1.sample('Nsamples',10,'perturbation',2);
    MsampleDoe(:,2)=Xdv2.sample('Nsamples',10,'perturbation',2);
    MsampleDoe(:,3)=Xdv3.sample('Nsamples',10);
    
    Xs=Samples('Xinput',Xin4,'MsamplesStandardNormalSpace',rand(10,4),'MsamplesDoeDesignVariables',MsampleDoe);
    Xs.MdoeDesignVariables=MsampleDoe;
    display(Xs)
    display(Xs.MdoeDesignVariables)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

if nargout>0
    % Export name of the UnitTest
    varargout{1}='Samples';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Samples (' datestr(now) ')'])
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

