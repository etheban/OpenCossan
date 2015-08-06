function varargout = unitTestConnector
%% UNIT TEST FOR CONNECTOR

Ntest=12;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
SworkDir = fullfile(OpenCossan.getCossanWorkingPath,'TestConnector');

%XEmod = Parameter('Sdescription','E-modulus','value',2.1E+11);
% changed to rv because of a newly discovered bug in Input. Will be changed
% back in future.
XEmod = RandomVariable('Sdistribution','normal','mean',0,'std',1);
Xrvset = RandomVariableSet('Cxrv',{XEmod},'Cmembers',{'XEmod'},'CXrandomvariables',{XEmod});
Xi = Input;
Xi = add(Xi,Xrvset);
Xi = Xi.sample();

Xinj = Injector('Sdescription', 'Injector for Unit test',...
    'Stype','scan', ....
    'SrelativePath','./', ...
    'Sscanfilepath', [OpenCossan.getCossanRoot '/examples/Unit_test/Connectors/Connector/'],...
    'Sscanfilename', 'InputFileTestConnector.cossan',...
    'Sfile','InputFileTestConnector.txt');

Xresp1 = Response('Sname', 'OUT1', ...
    'Sfieldformat', '%1d', ...
    'Svarname','', ...
    'Ncolnum',1, ...
    'Nrownum',2,...
    'Nrepeat',1);

Xe=Extractor('Sdescription','Extractor for Unit Test', ...
    'Srelativepath','./', ...
    'Sfile','InputFileTestConnector.txt',...
    'Xresponse',Xresp1);

%% 1
% Definition of Connector by passing the name of the executable

itest = 1;
try
    Xc = Connector('Sdescription', 'connector unit test',...
        'Smaininputpath',[OpenCossan.getCossanRoot '/examples/Unit_test/Connectors/Connector/'],...
        'Smaininputfile','InputFileTestConnector.txt',...
        'Sworkingdirectory',SworkDir,...
        'Ssolverbinary','/bin/echo', ...
        'Sexecmd','%Ssolverbinary 5 >> %Smaininputfile');
    
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
% Test whether the display works

itest = itest +1;
try
    Xc.display
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
% check the method test

itest = itest +1;
try
    Xc.test
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
% check the method deterministicAnalysis

itest = itest +1;
try
    Xc.deterministicAnalysis;
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 5
% Add a Matlab-script for preprocessing

itest = itest +1;
try
    Xc.SpreExecutionCommand = 'cp InputFileTestConnector.txt InputFileTestConnector.m';
    Xc.deterministicAnalysis;
    % test that the file InputFileTestConnector.m have been created by the
    % pre-execution command
    Tdir = dir(Xc.Sworkingdirectory) ;% get the name of the connector execution directory
    run(fullfile(Xc.Sworkingdirectory,Tdir(end).name,...
        'InputFileTestConnector'));
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 6
% Check the method inject
% This test depends on the succesfull execution of the previous test

itest = itest +1;
try
    Xinj.Srelativepath = Tdir(end).name; 
    Xc = Xc.add(Xinj);
    Xc.inject(Xi.getStructure);
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
% Check if the Injector Xinj is removed by using the method remove

itest = itest +1;
try
    Xc = remove(Xc,'Xinj');
    
    if ~ismember(Xc.CSmembersNames,'Xinj')
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
% Check the methos extract (it reads the number 5 written by the binary
% file defined in the constructor)
itest = itest +1;
try
    Xe.Srelativepath = Tdir(3).name; % set the relative path to the timestamp directory to test extract
    Xc = add(Xc,Xe);
    Tout = Xc.extract;
    if Tout.OUT1 == 5
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

Xe.Srelativepath = '.'; % re-set the relative path
%% 9
% Check if the Extractor Xe is removed by using the method remove

itest = itest +1;
try
    Xc = remove(Xc,'Xe');
    
    if ~ismember(Xc.CSmembersNames,'Xe')
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
% Check the method run

itest = itest +1;
try
    Xc = add(Xc,Xe);
    Xout = Xc.run(Xi);
    if Xout.Tvalues.OUT1 == 5
        Vtest(itest)=true;
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11

itest = itest +1;
try
    Xc = Xc.add(Xinj);
    Xc.inject(Xi.getStructure);
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 12
itest = itest +1;
try
    Tvalues(1).XEmod = 0.1;
    Tvalues(2).XEmod = 0.2;
    Tvalues(3).XEmod = 0.3;
    Xc.run(Tvalues);
    Vtest(itest)=true;
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% clean test execution folder
rmdir(SworkDir,'s');
%% Finalize the test

if nargout>0
    % Export name of the UnitTest
    varargout{1}='Connector';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Connector (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end

%% Remove simulation files
delete ('*.tgz')

end
