function varargout = unitTestJobManager
%% Unit Test of the JobManager object

Ntest=10;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
% the unit test must be executed in a network reachable folder
StestPath = fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/HighPerformanceComputing');

cd(StestPath)
%% Create objects required to test Samples object
% Create JobManagerInterface
Xjm = JobManagerInterface('Stype','GridEngine');
%% Test Constructor

%% 1
itest=1;
try
    Xg = JobManager('Sdescription','test #1',...
        'Squeue','pizzas64.q',...
        'Xjobmanagerinterface',Xjm);
    display(Xg)
    Vtest(itest)=true;
    Cmess{itest}='Constructor of the JobManager';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xg = JobManager('Sdescription','test #2');
    Cmess{itest}='This should fail because mandatory inputs are not provided';
catch ME
    % This should fail
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xg = JobManager('Sdescription','test #3','Xjobmanagerinterface',RandomVariable);
    Cmess{itest}='This should fail because wrong object passed to the constructor';
catch ME
    % This should fail
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 4
itest=itest+1;
try
    Xg = JobManager('Sdescription','test #4', ...
        'SjobName','JobName', ...
        'SpreExeCmd','echo preexecmd ', ...
        'SpostExeCmd','echo postexecmd ', ...
        'Xjobmanagerinterface',Xjm);
    display(Xg)
    Vtest(itest)=true;
    Cmess{itest}='Constructor using all the optional parameters';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    % Test Xdependent Field
    Xg = JobManager('Sdescription','test #6', ...
        'Xdependent',Xg, ...
        'Xjobmanagerinterface',Xjm);
    display(Xg)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 - Test method submitJob
itest=itest+1;
try
    % Test Xdependent Field
    Xg = JobManager('Sdescription','test #6','Xjobmanagerinterface',Xjm);
    CSjobID=Xg.submitJob('Sfoldername','test6');
    display(CSjobID)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7  - Test method submitJob with specified hostname
itest=itest+1;
try
    % Test Xdependent Field
    Xg = JobManager('Sdescription','test #7',...
        'Xjobmanagerinterface',Xjm,...
        'Squeue','pizzas64.q',...
        'Shostname','c810-cl21.uibk.ac.at');
    CSjobID = Xg.submitJob('Sfoldername','test7');
    CSstatus = Xg.getJobStatus('CSjobID',CSjobID);
    while strcmp(CSstatus{1},'pending')||strcmp(CSstatus{1},'running')
        pause(1)
        CSstatus = Xg.getJobStatus('CSjobID',CSjobID);
    end
    % open the .out file
    Nfid = fopen('test7.out','r');
    % skip the first two lines
    fgetl(Nfid);fgetl(Nfid);
    % check the hostname in the file
    Shost = fgetl(Nfid);
    fclose(Nfid);
    assert(strcmp('c810-cl21',Shost),'Job executed on the wrong host!')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
delete('test7.*')


%% 8
% this should fail since the hostname is not part of the specified queue
itest=itest+1;
try
    Xg = JobManager('Xjobmanagerinterface',Xjm,...
        'Squeue','pizzas64.q',...
        'Shostname','c810-cl20.uibk.ac.at');
    Xg.submit;
    Cmess{itest} = 'This test shall fail since Shostname is not part of the specified queue';
catch ME
    Vtest(itest) = true;
    Cmess{itest} = ME.message;
end

%% 9  - Test method deleteJob
itest=itest+1;
try
    Xg = JobManager('Sdescription','test #1',...
        'Squeue','pizzas64.q',...
        'Xjobmanagerinterface',Xjm,...
        'SpreExeCmd','echo preexecmd ', ...
        'SpostExeCmd','echo postexecmd ');
    [~,SuserName] = system('whoami'); SuserName=SuserName(1:end-1);
    CSjobID(1)=Xg.submitJob;
    Sstatus = Xg.deleteJob('CSjobID', CSjobID);
    assert(strcmp(Sstatus,[SuserName ' has deleted job ' CSjobID{1} 10]),...
        'Wrong output from job delete command')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
itest=itest+1;
try
    CSjobID(1)=Xg.submitJob;
    CSjobID(2)=Xg.submitJob;
    CSstatus = Xg.deleteJob('CSjobid', CSjobID);
    assert(strcmp(CSstatus{1},[SuserName ' has deleted job ' CSjobID{1} 10]) && ...
        strcmp(CSstatus{2},[SuserName ' has deleted job ' CSjobID{2} 10]),... % 10 is the ascii for the newline
        'Wrong output from job delete command')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message '\n'];
end

delete('test6.*')
delete('runCossanJobCossanXjob.sh')
% return to the original folder
cd(OpenCossan.getCossanWorkingPath)

%% Finalize the test
if nargout>0
    % Export name of the UnitTest
    varargout{1}='JobManager';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of JobManager (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
    
end
