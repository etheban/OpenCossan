%% Unit Test
% HMP 

function varargout=UnitTestSfemPolynomialChaos

%   This test verifies the implementation of the class "Perturbation". The 
%   example considered is a truss structure under static loading. The
%   structral matrices are transferred using a "Regular implementation",
%   i.e. the full system matrices are transferred.
%
%   The physical model refers to a truss structure of 11 bars subject to a
%   single load. The details on the physical model can be found at
%   http://www.mscsoftware.com/support/online_ex/previous_Nastran/Nas101/lesson_01.pdf
%   It should be noted that the model used for the unit testing is a
%   slightly modified version of this example, considering a single
%   vertical load at the tip node.
%
%   The Young's modulus of the bars of the upper chord of the truss
%   is modeled using a Gaussian random variable. The vertical force 
%   applied at the tip node is also modeled as a Gaussian random variable.
%
%   Using Perturbation, the mean and standard deviation of the
%   displacements of the truss at different nodes are calculated.
%
% Define the problem
RVKEE1     = RandomVariable('Sdescription','Youngs modulus','Sdistribution','normal','mean',1.76e6,'std',1.76e5); %#ok<*NASGU>
Xrvs       = RandomVariableSet('Cmembers',{'RVKEE1'},'CXrandomvariables',{RVKEE1});  
Xinp       = Input();       
Xinp       = add(Xinp,Xrvs);
Sdirectory = [OpenCossan.getCossanRoot '/examples/Unit_test/SFEM'];
Xinj       = Injector('Sscanfilepath',Sdirectory,'Sscanfilename','BEAM.dat','Sfile','BEAM.inp');
Xcon       = Connector('SpredefinedType','nastran_x86_64',...
                     'SmaininputPath',Sdirectory,...
                     'Smaininputfile','BEAM.inp');
Xcon   = add(Xcon,Xinj);
Xeval  = Evaluator('Xconnector',Xcon);
Xmodel = Model('Xevaluator',Xeval,'Xinput',Xinp);

% Test constructor
Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Galerkin',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
    Vtest(itest)=true;
    Cmess{itest}='Create SfemPolynomialchaos Object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xsfem.performAnalysis;
    Vtest(itest)=true;
    Cmess{itest}='Perform analysis with Galerkin P-C implementation';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Galerkin',...
        'Norder',-2,...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Guyan',...
        'Norder',3,...
        'Lautofactorization',false,...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
     Xsfem.performAnalysis;
     Cmess{itest}='Perform analysis with Guyan P-C implementation';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Galerkin',...
        'Norder',3,...
        'Vdroptolerancerange',[3],...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
    Vtest(itest)=true;
    Cmess{itest}='Wrong value for the parameter, no error, just warning, set to default';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Galerkin',...
        'Norder',3,...
        'convergenceparameter',-3,...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
    Vtest(itest)=true;
    Cmess{itest}='Wrong value for the parameter, no error, just warning, set to default';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Galerkin',...
        'Norder',3,...
        'droptolerance',-2,...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
    Vtest(itest)=true;
    Cmess{itest}='Wrong value for the parameter, no error, just warning, set to default';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Collocation',...
        'Norder',3,...
        'Sbasis','dummy',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
    Vtest(itest)=true;
    Cmess{itest}='Wrong value for the parameter, no error, just warning, set to default';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Collocation',...
        'MmasterDOFs',[4 1],...
        'Sgridtype','dummy',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
    Vtest(itest)=true;
    Cmess{itest}='Wrong value for the parameter, no error, just warning, set to default';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Guyan',...
        'Norder',3,...
        'MmasterDOFs',[4 1],...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
     Xsfem.performAnalysis;
     Vtest(itest)=true;
     Cmess{itest}='Perform analysis with Guyan P-C implementation';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11
itest=itest+1;
try
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Galerkin',...
        'Norder',3,...
        'Simplementation','Componentwise',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
     Xsfem.performAnalysis;
     Vtest(itest)=true;
     Cmess{itest}='Perform analysis with Galerkin P-C & Componentwise implementation';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12
itest=itest+1;
try
    Xsfem  = SfemPolynomialChaos('Xmodel',Xmodel,... 
        'Smethod','Collocation',...
        'Norder',3,...
        'Simplementation','Componentwise',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13
itest=itest+1;
try
    Xinj2       = Injector('Sscanfilepath',Sdirectory,'Sscanfilename','BEAM.dat','Sfile','BEAM.inp');
    Xcon2       = Connector('SpredefinedType','nastran_x86_64',...
                           'SmaininputPath',Sdirectory,...
                           'Smaininputfile','BEAM.inp');
    Xcon2   = add(Xcon2,Xinj2);
    Xresp1 = Response('Sname', 'disp', 'Sfieldformat', '%12e', ...
        'Clookoutfor',{'             4      G'},'Ncolnum',43,'Nrownum',1 );
    Xext   = Extractor('Sfile','BEAM.f06','CXresponse', {Xresp1});
    Xcon2   = add(Xcon2,Xext);
    Xeval2  = Evaluator('Xconnector',Xcon2);
    Xmodel2 = Model('Xevaluator',Xeval2,'Xinput',Xinp);
    Xsfem = SfemPolynomialChaos('Xmodel',Xmodel2,...
        'Smethod','Collocation',...
        'Norder',3,...
        'CyoungsmodulusRVs',{'RVKEE1'});
    Xsfem.performAnalysis;
    Vtest(itest)=true;
    Cmess{itest}='Perform analysis with Collocation P-C';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of SfemPolynomialChaos (' datestr(now) ')'])
disp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'])
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
varargout{1}='SfemPolynomialChaos';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
