%% Unit test for Polynomical Chaos

function varargout=UnitTestPolynomicalChaos

% Test constructor
Ntest=5;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Create an empty object
% Required by matlab build-in load and save 
itest = 1;
try
    Xpc = PolynomialChaos;
    display(Xpc)
    Vtest(itest)=true;
    Cmess{itest}='An empty PolynomialChaos object created successfully';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
load XsfemPC
itest = itest+1;
try
    Xpc = PolynomialChaos('Xsfem',XsfemPC,'Smethod','Galerkin','Norder',3); %#ok<*NASGU>
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest = itest+1;
try
    Xpc = PolynomialChaos('Xsfem',XsfemPC,'Norder',3); 
    Vtest(itest)=true;
    Cmess{itest}='A SfemPolynomialChaos object assigned to the PolynomialChaos';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest = itest+1;
%try
    Xpc = calibrate(Xpc);
    Vtest(itest)=true;
    Cmess{itest}='test the calibrate method (calculates coefficients of the expansion) with Galerkin option';
%catch ME
%    Cmess{itest}=[ME.identifier ' -- ' ME.message];
%end

%% 5
itest = itest+1;
try
    rv1     = RandomVariable('Sdistribution','normal','mean',0,'std',1);
    rv2     = RandomVariable('Sdistribution','normal','mean',0,'std',1);
    rv3     = RandomVariable('Sdistribution','normal','mean',0,'std',1);
    Xrvs    = RandomVariableSet('Cmembers',{'rv1','rv2','rv3'},'CXrandomvariables',{rv1 rv2 rv3});
    Xinp    = Input('Sdescription','Xinput object');
    Xinp    = add(Xinp,Xrvs);
    Xinp    = Xinp.sample('Nsamples',5000);
    Xsimout = apply(Xpc,Xinp);
    Vtest(itest)=true;
    Cmess{itest}='test the apply method (generates samples of response using the P-C as metamodel)';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of PolynomialChaos (' datestr(now) ')'])
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
varargout{1}='PolynomicalChaos';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
