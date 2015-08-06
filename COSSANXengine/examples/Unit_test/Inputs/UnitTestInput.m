function varargout = UnitTestInput
%%  Unit test for INPUT
%
% Prepared by HMP
%
%

Ntest          = 30;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);

%% Testing Random Variable Set
OpenCossan.resetRandomNumberGenerator(51125)

%% 1
itest=1;
try
    Xrv1  = RandomVariable('Sdistribution','normal','mean',100,'std',15);
    Xrv2  = RandomVariable('Sdistribution','normal','mean',100,'std',15);
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2});
    Xfun1 = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    Xpar  = Parameter('value',5);
    DV = DesignVariable('value',30,'minvalue',10,'maxvalue',50);
    Xin1   = Input;
    Xin1   = add(Xin1,Xrvs);
    Xin1   = add(Xin1,Xfun1);
    Xin1   = add(Xin1,Xpar);
    Xin1  = add(Xin1,DV);
    display(Xin1);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xin2   = remove(Xin1,Xfun1);
    display(Xin2);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xin3   = remove(Xin2,Xrvs,Xpar);
    display(Xin3);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 add random variable set with the same random variables as already present in Input
itest=itest+1;
try
    Xrvs1 = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2});
    Xin3   = add(Xin1,Xrvs1);
    Cmess{itest} = 'This test shall fail since both random variable sets contain the same random variables';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xin4   = Input('Sdescription','my Input','Xrvset',Xrvs,'Xfunction',Xfun1);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    Xin5 = sample(Xin4,'Nsamples',10);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    Xin6 = sample(Xin5,'Nsamples',5,'Ladd',true);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
itest=itest+1;
try
    Tin=getStructure(Xin6);
    display(Tin);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
itest=itest+1;
try
    Xo = evaluateFunction(Xin6);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
itest=itest+1;
try
    MX=getSampleMatrix(Xin6);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11
itest=itest+1;
try
    Pout = getValues(Xin6,'CSnames',{'Xrv1','Xrv2'});
    Cmess{itest}='Returns only the last requested value';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 12 - design variable
itest=itest+1;
try
    Xin7= Xin6.add(DV);
    Pout = getValues(Xin7,'Sname','DV');
    
    Xin7.CnamesDesignVariable
    Xin7.NdesignVariables
    Vtest(itest)=true;
    Cmess{itest}=' Interaction w/ DesignVariable ';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 - Add method for Input with Functions
itest=itest+1;

try
    
    RV1=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);
    RV2=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);
    Xrvs1=RandomVariableSet('Cmembers',{'RV1', 'RV2'},'CXrv',{RV1, RV2});
    Xpar=Parameter('value',1);
    Xfun=Function('Sexpression','<&Xpar&>+<&RV1&>');
    % Define Xinput
    Xin8 = Input('Sdescription','Input for mio','CXmembers',{Xrvs1 Xpar},'CSmembers',{'Xrvs1' 'Xpar'});
    Xin8 = add(Xin8,Xfun);
    Vtest(itest)=true;
    display(Xin8)
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 - Input with RandomVariableSet
itest=itest+1;
try
    rho=RandomVariable('Sdistribution','lognormal','mean',600,'std',140,'Sdescription','density');
    E=RandomVariable('Sdistribution','lognormal','mean',10e9,'std',1.6e9,'Sdescription','Young''s modulus');
    
    % Set of Random Variable Set
    Mcorrelation=eye(2);
    Mcorrelation(1,2)=0.8; % Add correlation between rho and E
    Mcorrelation(2,1)=0.8;
    Xrvset=RandomVariableSet('CXrandomVariables',{rho E},'CSmembers',{'rho' 'E'},'Mcorrelation',Mcorrelation);
    
    L=Parameter('value',1.8,'Sdescription','Beam Length');
    b=Parameter('value',0.12,'Sdescription','Beam width');
    maxDiplacement=Parameter('value',0.010,'Sdescription','Maximum allowed displacement');
    
    P=Parameter('value',5000);
    h=Parameter('value',0.24);
    
    % Definition of the Function
    I=Function('Sdescription','Moment of Inertia','Sexpression','<&b&>.*<&h&>.^3/12');
    
    Xin9=Input('CXmembers',{L b P h Xrvset I maxDiplacement},'CSmembers',{'L' 'b' 'P' 'h'  'Xrvset' 'I' 'maxDiplacement'});
    
    display(Xin9)
    % Add a meaningful error message
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15 - Test sample method
itest=itest+1;
try
    Xin10=Xin9.sample('Nsamples',10000);
    display(Xin10)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 16 - Check samples - Correlation
itest=itest+1;
try
    MHyperCube=Xin10.Xsamples.MsamplesHyperCube;
    Msns=Xin10.Xsamples.MsamplesStandardNormalSpace;
    Mphysical=Xin10.Xsamples.MsamplesPhysicalSpace;
    
    % Check size
    assert(size(MHyperCube,2)==Xin10.NrandomVariables, ...
        'UnitTest:Input','Sample matrix of wrong size')
    % Check correlation
    Mcorr=corr(Mphysical);
    assert(abs(Mcorr(1,2)-Mcorrelation(1,2))<0.05,...
        'UnitTest:Input','Wrong correlation in Physical Space')
    
    Mcorr=corr(MHyperCube);
    assert(abs(Mcorr(1,2)-Mcorrelation(1,2))<0.05,...
        'UnitTest:Input','Wrong correlation in HyperCube')
    
    Mcorr=corr(Msns);
    assert(abs(Mcorr(1,2))<0.02,...
        'UnitTest:Input','Wrong correlation in StandardNormalSpace')
    
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17 - Check samples - Mapping
itest=itest+1;
try
    Msamples=randn(100,Xin10.NrandomVariables);
    
    Mphysical=Xin10.map2physical(Msamples);
    
    Msamples2=Xin10.map2stdnorm(Mphysical);
    
    assert(logical(max(abs(Msamples(1)-Msamples2(1)))<1e-10), ...
        'UnitTest:Input','Wrong Mapping') %FIXME 
    
    Msamples=rand(100,Xin10.NrandomVariables);
    
    Mphysical=Xin10.cdf2physical(Msamples);
    
    MsamplesSNS=Xin10.cdf2stdnorm(Msamples);
    
    MsamplesSNS2=Xin10.map2stdnorm(Mphysical);
    
    assert(logical(max(abs(MsamplesSNS(1)-MsamplesSNS2(1)))<1e-10), ...
        'UnitTest:Input','Wrong Mapping')
    
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 18 - Input with Discrete RandomVariable
itest=itest+1;
try
    % Add a Discrete Random Variable
    XrvD=RandomVariable('Sdistribution','poisson','mean',20);
    
    Mcorrelation=eye(3);
    Mcorrelation(1,2)=0.1; % Add correlation between rho and E
    Mcorrelation(2,1)=0.1;
    Xrvset=RandomVariableSet('CXrandomVariables',{rho E XrvD},'CSmembers',{'rho' 'E' 'XrvD'},'Mcorrelation',Mcorrelation);
    
    Xin11=Input('CXmembers',{L b P h Xrvset I maxDiplacement},'CSmembers',{'L' 'b' 'P' 'h'  'Xrvset' 'I' 'maxDiplacement'});
    
    display(Xin11)
    % Add a meaningful error message
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% 19 - Test sample method
itest=itest+1;
try
    Xin12=Xin11.sample('Nsamples',100);
    display(Xin12)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 20 - Check samples - Correlation
itest=itest+1;
try
    MHyperCube=Xin12.Xsamples.MsamplesHyperCube;
    Msns=Xin12.Xsamples.MsamplesStandardNormalSpace;
    Mphysical=Xin12.Xsamples.MsamplesPhysicalSpace;
    
    % Check size
    assert(size(MHyperCube,2)==Xin12.NrandomVariables, ...
        'UnitTest:Input','Sample matrix of wrong size')
    % Check correlation
    Mcorr=corr(Mphysical);
    assert(abs(Mcorr(1,2)-Mcorrelation(1,2))<0.15,...
        'UnitTest:Input','Wrong correlation in Physical Space')
    
    Mcorr=corr(MHyperCube);
    assert(abs(Mcorr(1,2)-Mcorrelation(1,2))<0.15,...
        'UnitTest:Input','Wrong correlation in HyperCube')
    
    Mcorr=corr(Msns);
    assert(abs(Mcorr(1,2))<0.12,...
        'UnitTest:Input','Wrong correlation in StandardNormalSpace')
    
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 21 - Check samples - Mapping
itest=itest+1;
try
    Msamples=randn(100,Xin12.NrandomVariables);
    
    Mphysical=Xin12.map2physical(Msamples);
    
    Msamples2=Xin12.map2stdnorm(Mphysical);
    
    assert(logical(max(abs(Msamples(1)-Msamples2(1)))<1e-10), ...
        'UnitTest:Input','Wrong Mapping') %fixme
    
    Msamples=rand(100,Xin12.NrandomVariables);
    
    Mphysical=Xin12.cdf2physical(Msamples);
    
    MsamplesSNS=Xin12.cdf2stdnorm(Msamples);
    
    MsamplesSNS2=Xin12.map2stdnorm(Mphysical);
    
    assert(logical(max(abs(MsamplesSNS(1)-MsamplesSNS2(1)))<1e-10), ...
        'UnitTest:Input','Wrong Mapping')
    
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 22 - Test with GaussianRandomVariableSet
itest=itest+1;
try
    
    
    N      = 20;
    A1     = [2 -1.8 0; -1.8 2 0; 0 0 1];
    A2     = [2 -1.9 1.9; -1.9 2 -1.9; 1.9 -1.9 2];
    A3     = [2 1.9 0;1.9 2 0; 0 0 1];
    p      = [0.03 0.95 0.02];
    MU     = [4 4 -4;-3 -5 4;4 -4 0];
    SIGMA  = cat(3,A1,A2,A3);
    obj    = gmdistribution(MU,SIGMA,p);
    r      = random(obj,N);
    
    Xgmrvs = GaussianMixtureRandomVariableSet('MdataSet',r,'Cmembers',{'X1' 'X2' 'X3'});
    
    Xin13=Xin12.add(Xgmrvs);

    display(Xin13)
    % Add a meaningful error message
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 23 - Check samples
itest=itest+1;
try
    assert(Xin13.Nsamples==0,'UnitTest:Input','Samples can not be present in the Input object')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 24 - Check sample method
itest=itest+1;
try
    Xin14=Xin13.sample('Nsamples',100);
    
    assert(size(Xin14.Xsamples.MsamplesPhysicalSpace,2)==Xin14.NrandomVariables, ...
        'UnitTest:Input','Wrong number of samples')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 25 - check merge method
itest=itest+1;
try
    Xin15 = Xin1.merge(Xin9);
    display(Xin15)
    
    Xin16 = Xin1.merge(Xin8); % the parameter value is the same for both Input objects
    assert(Xin16.Xparameters.Xpar.value==Xin1.Xparameters.Xpar.value, ...
        'UnitTest:Input','The parameter value of Xin1 shall be taken')
    Xin17 = Xin8.merge(Xin1);
    assert(Xin17.Xparameters.Xpar.value==Xin8.Xparameters.Xpar.value, ...
        'UnitTest:Input','The parameter value of Xin8 shall be taken')

    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 26 - check merge method - with Stochastic Process
itest=itest+1;
try
    Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
        'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
        'Cinputnames',{'t1','t2'},... % Define the inputs
        'Sscript','sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov = sigma^2*exp(-1/b*abs(Tinput(i).t2-Tinput(i).t1)); end', ...
        'Coutputnames',{'fcov'}); % Define the outputs
    SP1    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xcovfun,'Mcoord',0:0.25:2);
    Xin18 = Input('CXmembers',{SP1},'Csmembers',{'SP1'});
    
    Xin19 = Xin18.merge(Xin1);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 27 - check merge method - with Input object containing samples
itest=itest+1;
try    
    Xin20 = Xin6.merge(Xin8);
    assert(isempty(Xin20.Xsamples),'UnitTest:Input','The samples shall not be merged')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 28 - check merge method - with Input object containing samples
itest=itest+1;
try 
    Xrvs21 = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2});
    Xin21 = Input;
    Xin21   = add(Xin21,Xrvs21);
    Xin22 = Xin21.merge(Xin1);
    
    Cmess{itest} = 'This test shall fail since the random variables in both random variable sets have the same names';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 29 - check set method
itest=itest+1;
try 
    Xin = Input('CXmembers',{Xrvs},'CSmembers',{'Xrvset'});
    Xin   = Xin.set('SobjectName','Xrv1','SpropertyName','std','value',2);
    Vtest(itest)=true;
    display(Xin)
catch ME
    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 30 - check set method
itest=itest+1;
try 
    Xrvs = RandomVariableSet('Cmembers',{'XrvA','XrvB'},'CXrv',{Xrv1,Xrv2});
    Xin = Input('CXmembers',{Xrvs},'CSmembers',{'Xrvset'});
    Xin   = Xin.set('SobjectName','XrvA','SpropertyName','std','value',2);
    Vtest(itest)=true;
    display(Xin)
catch ME
    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


    
    
if nargout>0
    % Export name of the UnitTest
    varargout{1}=['Unit Test of INPUT (' datestr(now) ')'];
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;    
else
    %% Show summary of the test
    
    disp('  ')
    disp('  ')
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of INPUT (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
    
    %% FINISHED - Go home !
    
end








