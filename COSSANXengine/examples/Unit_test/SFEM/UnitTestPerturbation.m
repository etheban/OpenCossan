%% Unit Test
% HMP 

function varargout=UnitTestPerturbation

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
RVF1       = RandomVariable('Sdescription','loading','Sdistribution','normal','mean',1.5e3,'std',1.5e2);
RVM1       = RandomVariable('Sdescription','point mass','Sdistribution','normal','mean',10.,'std',1.);
Xrvs       = RandomVariableSet('Cmembers',{'RVKEE1','RVF1'},'CXrandomvariables',{RVKEE1 RVF1});  
Xinp       = Input();       
Xinp       = add(Xinp,Xrvs);
Sdirectory = [OpenCossan.getCossanRoot '/examples/Unit_test/SFEM'];
Xinj       = Injector('Sscanfilepath',Sdirectory,'Sscanfilename','TRUSS01.dat','Sfile','TRUSS01.inp');
Xcon       = Connector('SpredefinedType','nastran_x86_64',...
                     'SmaininputPath',Sdirectory,...
                     'Smaininputfile','TRUSS01.inp');
Xcon   = add(Xcon,Xinj);
Xeval  = Evaluator('Xconnector',Xcon);
Xmodel = Model('Xevaluator',Xeval,'Xinput',Xinp);

% Test constructor
Ntest=8;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1
itest=itest+1;
try
    Xpert01 = Perturbation('Xmodel',Xmodel,...  %define target model
        'Sanalysis','Static',...                    %specify that analysis type is static
        'Simplementation','Regular',...             %specifies use of "regular implementation"
        'CyoungsmodulusRVs',{'RVKEE1'},...
        'CforceRVs',{'RVF1'});  
    Vtest(itest)=true;
    Cmess{itest}='Create Perturbation Object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xpert01.performAnalysis;
    Vtest(itest)=true;
    Cmess{itest}='Perform statis analysis with regular implementation';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xpert01 = Perturbation('Xmodel',Xmodel,...  %define target model
        'Sanalysis','Static',...    %specify that analysis type is static
        'Simplementation','Componentwise',...
        'CyoungsmodulusRVs',{'RVKEE1'},...
        'CforceRVs',{'RVF1'});  
    Xpert01.performAnalysis;
    Vtest(itest)=true;
    Cmess{itest}='Perform statis analysis with componentwise implementation';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xrvs       = RandomVariableSet('Cmembers',{'RVKEE1','RVM1'},'CXrandomvariables',{RVKEE1 RVM1});  
    Xinp       = Input();       
    Xinp       = add(Xinp,Xrvs);
    Xinj       = Injector('Sscanfilepath',Sdirectory,'Sscanfilename','TRUSS02.dat','Sfile','TRUSS02.inp');
    Xcon       = Connector('SpredefinedType','nastran_x86_64',...
                     'Smaininputfile',[Sdirectory filesep 'TRUSS02.inp']);
    Xcon    = add(Xcon,Xinj);
    Xeval   = Evaluator('Xconnector',Xcon);
    Xmodel  = Model('Xevaluator',Xeval,'Xinput',Xinp);
    Xpert02 = Perturbation('Xmodel',Xmodel,...  %define target model
        'Sanalysis','Modal',...    %specify that analysis type is static
        'Simplementation','Regular',...
        'CyoungsmodulusRVs',{'RVKEE1'},...
        'CdensityRVs',{'RVM1'});  %pass name of Nastran input file containing identifiers
    Xpert02.performAnalysis;
    Vtest(itest)=true;
    Cmess{itest}='Perform modal analysis with regular implementation';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xrvs       = RandomVariableSet('Cmembers',{'RVKEE1','RVM1'},'CXrandomvariables',{RVKEE1 RVM1});  
    Xinp       = Input();       
    Xinp       = add(Xinp,Xrvs);
    Xpert02 = Perturbation('Xmodel',Xmodel,...  %define target model
        'Sanalysis','Modal',...    %specify that analysis type is static
        'Simplementation','Componentwise',...
        'CyoungsmodulusRVs',{'RVKEE1'},...
        'CdensityRVs',{'RVM1'});  %pass name of Nastran input file containing identifiers
    Xpert02.performAnalysis;
    Vtest(itest)=true;
    Cmess{itest}='Perform modal analysis with componentwise implementation';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    Xpert03 = Perturbation('Xmodel',Xmodel,...  %define target model
        'Sanalysis','Dynamic',...    %specify that analysis type is static
        'Simplementation','Componentwise',...
        'CyoungsmodulusRVs',{'RVKEE1'},...
        'CdensityRVs',{'RVM1'}); 
    Xpert03.performAnalysis;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 7
itest=itest+1;
try
    Xpert03 = Perturbation('Xmodel',Xmodel,...  %define target model
        'Sanalysis','Modal',...    %specify that analysis type is static
        'Simplementation','Dummy',...
        'CyoungsmodulusRVs',{'RVKEE1'},...
        'CdensityRVs',{'RVM1'}); 
    Xpert03.performAnalysis;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 8
itest=itest+1;
try
    Xpert03 = Perturbation('Xmodel',Xmodel,...  %define target model
        'Sanalysis','Modal',...    %specify that analysis type is static
        'Simplementation','Regular'); 
    Xpert03.performAnalysis;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of Perturbation (' datestr(now) ')'])
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
varargout{1}='Perturbation';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
