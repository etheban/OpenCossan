%%  Reference
%   class 'TimeSeries'
function varargout = UnitTestDataseries

Ntest=24;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
iTest=1;


%% 1 constructor, creating elements one by one
try
    Xds(1,1) = Dataseries('Sdescription','wgeöofqwelö','Mcoord',1:100,'Vdata',rand(1,100),'Sindexname','fleubleubleu','Sindexunit','ihgbrg');
    Xds(2,1) = Dataseries('Mcoord',1:100,'Vdata',rand(1,100),'Sindexname','frequency','Sindexunit','Hz');
    Xds(1,2) = Dataseries('Mcoord',1:5,'Vdata',rand(1,5),'Sindexname','frequency','Sindexunit','Hz');
    Xds(2,2) = Dataseries('Mcoord',1:5,'Vdata',rand(1,5),'Sindexname','frequency','Sindexunit','Hz');
    Vtest(iTest)=true;
    Cmess{iTest}='Successfull Test Constructor #1';
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 2 constructor, creating the full matrix at once
iTest = iTest+1;
try
    XdsBig = Dataseries('Sdescription','Test Dataseries matrix',...
        'CSindexName',{'t1','t2','t3','t4'},...
        'CSindexUnit',{'s','s','s','s'},...
        'CMcoord',{1:10, 1:20, 1:30, 1:40},...
        'CVdata',[mat2cell(randn(100,10),ones(1,100)),...
        mat2cell(randn(100,20),ones(1,100)), ...
        mat2cell(randn(100,30),ones(1,100)), ...
        mat2cell(randn(100,40),ones(1,100))]);
    Vtest(iTest)=true;
    Cmess{iTest}='Successfull Test Constructor #1';
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 3 display
iTest = iTest+1;
try
    display(Xds(1,1))
    display(Xds(2,1))
    display(Xds(1,2))
    display(Xds(2,2))
    display(Xds)
    Cmess{iTest}='Test Display';
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 4 change description
iTest = iTest+1;
try
    prev = Xds(1,1).Sdescription;
    Xds(1,1).Sdescription = 'changed';
    if strcmp(prev,Xds(1,1).Sdescription)
        error('Sdescription was not changed')
    end
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end
    
%% 5 change index name
iTest = iTest+1;
try
    prev = Xds(1,1).SindexName;
    Xds(1,1).SindexName = 'changed';
    if strcmp(prev,Xds(1,1).SindexName)
        error('SindexName of Xds(1,1) was not changed')
    end
    if ~strcmp('frequency',Xds(1,2).SindexName)
        error('SindexName of Xds(1,2) was wrongly changed')
    end
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end
 
%% 6 change index unit
iTest = iTest+1;
try   
    prev = Xds(1,1).SindexUnit;
    Xds(1,1).SindexUnit = 'changed';
    if strcmp(prev,Xds(1,1).SindexUnit)
        error('SindexUnit was not changed')
    end
    if ~strcmp('Hz',Xds(1,2).SindexUnit)
        error('SindexName of Xds(1,2) was wrongly changed')
    end
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 7 change Vdata
iTest = iTest+1;
try
    VoldData = Xds(1,2).Vdata;
    Xds(1,2).Vdata = rand(1,5);
    VnewData = Xds(1,2).Vdata;
    if isequal(VoldData,VnewData)
        error('Vdata attribute was not changed')
    end
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 8 change Vdata - wrong Vdata
iTest = iTest+1;
try
    Xds(1,2).Vdata = rand(50,1);
    Xds(1,2).Vdata;
    Cmess{iTest}='Vdata attribute was changed w/o error message';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 10 change Mcoord
iTest = iTest+1;
try
    MoldCoord = Xds(1,2).Mcoord;
    Xds(1,2).Mcoord = rand(2,5);
    MnewCoord = Xds(1,2).Mcoord;
    if isequal(MoldCoord,MnewCoord)
        error('Mcoord attribute was not changed')
    end
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];    
end

%% 11 change Mcoord - wrong Mcoord
iTest = iTest+1;
try
    Xds(1,2).Mcoord = rand(50,1);
    Xds(1,2).Mcoord;
    Cmess{iTest}='Mcoord attribute was changed w/o error message';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    
end

%% 12 adddata
iTest = iTest+1;
Xds2 = Dataseries('Mcoord',1:100,'Vdata',rand(1,100),'Sindexname','frequency','Sindexunit','Hz');
try
    Xds = addData(Xds2,'Mcoord',101:110,'Vdata',rand(1,10));
    display(Xds)
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 13 adddata, wrong Mcoord
iTest = iTest+1;
try
    Xds = addData(Xds2,'Mcoord',102:110,'Vdata',rand(1,10));
    display(Xds)
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 14 adddata, repeated Mcoord
iTest = iTest+1;
try
    Xds = addData(Xds2,'Mcoord',1:10,'Vdata',rand(1,10));
    display(Xds)
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 15 adddata, wrong Vdata
iTest = iTest+1;
try
    Xds = addData(Xds2,'Mcoord',101:110,'Vdata',rand(10,1));
    display(Xds)
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 16 adddata w/ multiple samples
iTest = iTest+1;
Xds3 = XdsBig(:,1);
try
    Xds = addData(Xds3,'Mcoord',21:30,'Mdata',rand(100,10));
    display(Xds)
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 17 adddata w/ multiple samples, wrong Vdata
iTest = iTest+1;
Xds3 = XdsBig(:,1);
try
    Xds = addData(Xds3,'Mcoord',101:110,'Vdata',rand(100,1));
    display(Xds)
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% 18 invalid input argument
iTest = iTest+1;
try
    Xds(1,1) = Dataseries('Sdhdfdhfn','wgeöofqwelö','Mcoord',1:100,'Vdata',rand(1,100),'Sindexname','fleubleubleu','Sindexunit','ihgbrg');
    Cmess{iTest}='This should fail';
    display(Xds(1,1))
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% test horzcat, vertcat
%% test horzcat
iTest = iTest+1;
try
    Xds1=XdsBig(:,1);
    Xds2=XdsBig(:,2);
    
    temp=[Xds1,Xds2];
    assert(size(Xds1,2)+size(Xds2,2)==size(temp,2),'','Wrong number of columns after horzcat')
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];    
end
%% test horzcat
iTest = iTest+1;
try
    Xds1=XdsBig(:,1);
    Xds2=XdsBig(1:50,2);
    
    temp=[Xds1,Xds2];
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];    
end
%% test vertcat
iTest = iTest+1;
try
    Xds1=XdsBig(:,1);
    Xds3=Xds1(1,1);
    
    temp=[Xds1;Xds3];
    assert(size(Xds1,1)+size(Xds3,1)==size(temp,1),'','Wrong number of rows after vertcat')
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];    
end

%% test vertcat (should fail)
iTest = iTest+1;
try
    Xds3.Mcoord = Xds3.Mcoord+1;
    
    temp=[Xds1;Xds3];
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];    
end

%% test vertcat
iTest = iTest+1;
try
    Xds4=XdsBig(:,1:2);
    Xds5=XdsBig(1,1:2);
    
    temp=[Xds4;Xds5];
    assert(size(Xds4,1)+size(Xds5,1)==size(temp,1),'','Wrong number of rows after vertcat')
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%% test vertcat
iTest = iTest+1;
try
    Xds4=XdsBig(:,1:2);
    Xds6=XdsBig(1,2:3);
    
    temp=[Xds4;Xds6];
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];    
end

%% test vertcat
iTest = iTest+1;
try
    Xds1=XdsBig(:,1);
    Xds6=XdsBig(1,2:3);
    
    temp=[Xds1;Xds6];
    Cmess{iTest}='This should fail';
catch ME
    Vtest(iTest)=true;
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];    
end

%%
if nargout>0
    % Export name of the UnitTest
    varargout{1}='Dataseries';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of Dataseries (' datestr(now) ')'])
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



