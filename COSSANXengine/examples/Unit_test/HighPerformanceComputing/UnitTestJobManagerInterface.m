function varargout = UnitTestJobManagerInterface

Ntest=12;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Test Constructor
itest=1;
try
    Xjm = JobManagerInterface('Stype','GridEngine_matlab');
    display(Xjm)
    Vtest(itest)=true;
catch ME
    Cmess{itest} = ME.message;
end

%%  2 Test Constructor with wrong stype
itest=itest+1;
try
    Xjm = JobManagerInterface('Stype','GGridEngine_matlab');
catch ME
    Vtest(itest)=true;
    Cmess{itest} = ME.message;
end

%% 3 Test Constructor (old bug was ignoring Sdescription)
itest=itest+1;
try
    Xjm = JobManagerInterface('Sdescription','Interface for Unit test',...
        'Stype','GridEngine_matlab');
    assert(strcmp(Xjm.Sdescription,'Interface for Unit test'),...
        'Sdescription not correct')
    Vtest(itest)=true;
catch ME
    Cmess{itest} = ME.message;
end

%% 4 Test method getQueues
itest=itest+1;
try
    CSqueues = Xjm.getQueues;
    CSexpected ={'FEAP' 'NASTRAN' 'ABAQUS' 'ANSYS'}'; % change this if the grid configuration is changed!!!
    assert(all(ismember(CSexpected,CSqueues)),'Wrong queues names retrieved')
    Vtest(itest)=true;
catch ME
    Cmess{itest} = ME.message;
end

%% 5 Test method getHosts
itest=itest+1;
try
    CShosts = Xjm.getHosts('SqueueName','pizzas64.q');
    CSexpected ={    'c810-cl21.uibk.ac.at';...
        'c810-cl22.uibk.ac.at';...
        'c810-cl23.uibk.ac.at';...
        'c810-cl24.uibk.ac.at';...
        'c810-cl25.uibk.ac.at'}; % change this if the grid configuration is changed!!!
    assert(all(strcmp(CSexpected,CShosts)),'Wrong host names retrieved')
    Vtest(itest)=true;
catch ME
    Cmess{itest} = ME.message;
end


%% 6 wrong queue name
itest=itest+1;
try
    CShosts = Xjm.getHosts('SqueueName','non_existent');   
    assert(logical(isempty(CShosts)),'Non existing queue should return an emmpty variable')
    Vtest(itest)=true;
catch ME    
    Cmess{itest} = ME.message;
end

%% 7. Test method checkHost
itest=itest+1;
try
    Lavailable = Xjm.checkHost('ShostName','c810-cl21.uibk.ac.at');    
    assert(Lavailable,'Either c810-cl21 is not available or the method failed')
    Vtest(itest)=true;
catch ME
    Cmess{itest} = [ME.message ' This is failing when no Squeuname is spcified'];
end

%% 8
itest=itest+1;
try
    Lavailable = Xjm.checkHost('ShostName','c810-cl21.uibk.ac.at',...
        'Squeuename','pizzas64.q');    
    assert(Lavailable,'Either c810-cl21 is not available or the method failed')
    Vtest(itest)=true;
catch ME
    Cmess{itest} = [ME.message ' This is failing when no Squeuname is spcified'];
end

%% 9
itest=itest+1;
try
    Lavailable = Xjm.checkHost('ShostName','somename');    
    assert(Lavailable,'This machine does not exist')
catch ME
    Vtest(itest)=true;
    Cmess{itest} = ME.message;
end

%% 10 test method getSlotNumber
itest=itest+1;
try
    Nslots = Xjm.getSlotNumber('Squeuename','pizzas64.q');
    Vtest(itest)=true;
catch ME
    Cmess{itest} = ME.message;
end

%% 11 test getSlotNumber without squeuename
itest=itest+1;
try
    Nslots = Xjm.getSlotNumber('Shostname','c810-cl25.uibk.ac.at') 
catch ME
    Vtest(itest)=true;
    Cmess{itest} = ME.message;
end

%% 12 test getSlotNumber with a non-existing hostname
itest=itest+1;
try
    Nslots = Xjm.getSlotNumber('Squeuename','pizzas64.q','Shostname','somehost') 
catch ME
    Vtest(itest)=true;
    Cmess{itest} = ME.message ;
end
%% Finalize the test
if nargout>0
    % Export name of the UnitTest
    varargout{1}='JobManagerInterface';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of JobManagerInterface (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end
