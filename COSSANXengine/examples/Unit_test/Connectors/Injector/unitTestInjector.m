function varargout = unitTestInjector

%% Unit test for the injector
Ntest=7;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

StestPath = [OpenCossan.getCossanRoot '/examples/Unit_test/Connectors/Injector/'];

% 1. Prepare Input object to be used for testing the Injector
Emod = RandomVariable('Sdistribution','normal','mean',2.1E+11,'std',2.1E+10);
nu = RandomVariable('Sdistribution','normal','mean',0.3,'std',0.03);
t = RandomVariable('Sdistribution','normal','mean',0.01,'std',0.001);
rho = RandomVariable('Sdistribution','normal','mean',7800,'std',780);

Xrvset = RandomVariableSet('Cmembers',{'Emod','nu','t','rho'},'CXrv',{Emod,nu,t,rho});

Xi = Input;
Xi = add(Xi,Xrvset);

TsamplesPhysicalSpace.Emod = 1.7583399048e+11;
TsamplesPhysicalSpace.nu = 0.3;
TsamplesPhysicalSpace.t=1.0881851415e-02;
TsamplesPhysicalSpace.rho = 8056.1;


Xsmp1  = Samples('Xinput',Xi,'TsamplesPhysicalSpace',TsamplesPhysicalSpace);
Xi = Input('Xrvset',Xrvset,'Xsamples',Xsmp1);

%% 1. Check property Sdescription

itest = 1;
try
    Xinj = Injector('Sdescription', 'Injector for Unit test',...
        'Stype','scan', ....
        'Sscanfilepath',StestPath,...
        'Sscanfilename', 'properties.cossan',...
        'Sworkingdirectory',StestPath,...
        'Sfile','properties.dat');
    
    if ~strcmp(Xinj,'Injector for Unit test')
        Vtest(itest)=true;
    end
catch ME
    Cmess{itest} = ME.message;
end


%% 2. Check if it works if file to be scanned is in another directory

itest = itest + 1;
try
    Xinj = Injector('Sdescription', 'Injector for Unit test',...
        'Stype','scan', ....
        'Sscanfilepath',[StestPath 'ScanFilePath'],...
        'Sscanfilename', 'properties2.cossan',...
        'Sworkingdirectory',StestPath,...
        'Srelativepath','ScanFilePath', ...
        'Sfile','properties2.dat');
    
    inject(Xinj,Xi)
    Nfid = fopen([StestPath 'ScanFilePath/properties2.dat']);
    fclose(Nfid);
    Vtest(itest)=true;
    
catch ME
    Cmess{itest} = ME.message;
end

%% 3 Inject values as passed to the Input object

itest = itest + 1;
try
    Xinj = Injector('Sdescription', 'Injector for Unit test',...
        'Stype','scan', ....
        'Sscanfilepath',StestPath,...
        'Sscanfilename', 'properties.cossan',...
        'Sworkingdirectory',StestPath,...
        'Sfile','properties3.dat');
    
    inject(Xinj,Xi)
    SFileComparison = visdiff([StestPath 'CorrectlyInjectedFiles/properties.dat'],...
        [StestPath 'properties3.dat']);
    if isempty(strfind(SFileComparison,'unmatched'))
        Vtest(itest)=true;
    else
        Cmess{itest} = 'File with injected values does not correspond to the target file';
    end
catch ME
    Cmess{itest} = ME.message;
end


%% 4 Inject original values

itest = itest + 1;
try
    Xinj = Injector('Sdescription', 'Injector for Unit test',...
        'Stype','scan', ....
        'Sscanfilepath',StestPath,...
        'Sscanfilename', 'properties.cossan',...
        'Sfile','properties4.dat');
    
    
    Xcdummy = Connector('SpredefinedType','nastran_i386','Smaininputfile','test.inp',...
        'Smaininputpath',StestPath,'Sworkingdirectory',StestPath);
    Xcdummy.Ssolverbinary = 'cat';
    Xcdummy = Xcdummy.add(Xinj);
    Xcdummy.deterministicAnalysis;
    
    SFileComparison = visdiff([StestPath 'CorrectlyInjectedFiles/properties_original.dat'],...
        [StestPath 'properties4.dat']);
    if isempty(strfind(SFileComparison,'unmatched'))
        Vtest(itest)=true;
    else
        Cmess{itest} = 'File with injected values not written correctly';
    end
catch ME
    Cmess{itest} = ME.message;
end

%% test injector with StochasticProcess
Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
          'Lfunction',true,'Liostructure',true,'Liomatrix',false,...
          'Cinputnames',{'t1','t2'},... % Define the inputs 
          'Spath',StestPath,...
          'Sfile','expcovfunction.m',... % external file
          'Coutputnames',{'fcov'}); % Define the outputs

time   = 0:0.05:0.5;  
SP1    = StochasticProcess('Sdistribution','normal','Vmean',1.0,'Xcovariancefunction',Xcovfun,'Mcoord',time);
SP1    = KL_terms(SP1,'NKL_terms',5,'Lcovarianceassemble',false);

Mdata = [1.2597160e+00, 1.1887828e+00, 9.7755398e-01, 8.2960914e-01, ...
         8.2150161e-01, 8.6554467e-01, 8.8637816e-01, 9.4003183e-01, ...
         1.1033306e+00, 1.3002285e+00, 1.3353142e+00];
Xds = Dataseries('Mcoord',time,'Mdata',Mdata);

Xin     = Input;
Xin    = add(Xin,SP1);

Xsmp1  = Samples('Xinput',Xin,'Xdataseries',Xds);
Xin = Input('Xstochasticprocess',SP1,'Xsamples',Xsmp1);

%% 5
itest = itest + 1;
try
    Xinj = Injector('Sdescription', 'Injector for Unit test',...
                'Stype','scan', ....
                'Sscanfilename', 'UnitTestAbaqus.cossan',...
                'Sscanfilepath',StestPath,...
                'Sfile','UnitTestAbaqus.dat');
    display(Xinj);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 Injector for Abaqus

itest=itest+1;
try
    Xinj = Injector('Sdescription', 'Injector for Unit test',...
                'Stype','scan', ....
                'Sscanfilename', 'UnitTestAbaqus.cossan',...
                'Sscanfilepath',StestPath,...
                'Sworkingdirectory',StestPath,...
                'Sfile','UnitTestAbaqus.dat');
    inject(Xinj,Xin);
    SFileComparison = visdiff([StestPath 'CorrectlyInjectedFiles/SP1Abaqus.txt'],...
        [StestPath 'SP1Abaqus.txt']);
    if isempty(strfind(SFileComparison,'unmatched'))
        Vtest(itest)=true;
    else
        Cmess{itest} = 'File with injected values not written correctly';
    end          
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 Injector for Nastran

itest=itest+1;
try
    Xinj = Injector('Sdescription', 'Injector for Unit test',...
                'Stype','scan', ....
                'Sscanfilename', 'UnitTestNastran16.cossan',...
                'Sscanfilepath',StestPath,...
                'Sworkingdirectory',StestPath,...
                'Sfile','UnitTestNastran16.dat');
    inject(Xinj,Xin);
    SFileComparison = visdiff([StestPath 'CorrectlyInjectedFiles/SP1Nastran16.txt'],...
        [StestPath 'SP1Nastran16.txt']);
    if isempty(strfind(SFileComparison,'unmatched'))
        Vtest(itest)=true;
    else
        Cmess{itest} = 'File with injected values not written correctly';
    end          
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Remove created files
delete('*.dat')

%% Finalize the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='Injector';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Injector (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end



end
