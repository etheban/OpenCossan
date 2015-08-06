%% Unit Test
% HMP 

function varargout=UnitTestNastsem

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
Ntest=8;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1
itest=itest+1;
try
    Xsfem = Nastsem('Xmodel',Xmodel,'Smethod','Perturbation','Vfixednodes',[1],...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
    Vtest(itest)=true;
    Cmess{itest}='Create Nastsem Object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xsfem.performAnalysis;
    Vtest(itest)=true;
    Cmess{itest}='Perform analysis with nastsem object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xsfem = Nastsem('Smethod','Perturbation',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xsfem = Nastsem('Xmodel',Xmodel,... 
        'Smethod','Perturbation'); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xsfem = Nastsem('Xmodel',Xmodel,... 
        'Smethod','Perturbation',...
        'CyoungsmodulusRVs',{'dummy'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    Xsfem = Nastsem('Xmodel',Xmodel,... 
        'Smethod','dummy',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    Xsfem = Nastsem('Xmodel',Xmodel,... 
        'Smethod','Perturbation',...
        'CyoungsmodulusRVs',{'RVKEE1'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
itest=itest+1;
try
    Xsfem = Nastsem('Xmodel',Xmodel,... 
        'Smethod','Perturbation',...
        'CforceRVs',{'RVKEE1'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of Nastsem (' datestr(now) ')'])
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
varargout{1}='Nastsem';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
